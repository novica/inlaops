#' parsnip model constructor for inlabru fixed-effects models
#'
#' Scoped to models with fixed effects and IID random intercepts only.
#' Spatial models with BYM2, SPDE, AR1, or LGCP likelihoods should use
#' inlabru's native interface.
#'
#' The formula passed to `parsnip::fit()` must use inlabru component syntax.
#' An explicit intercept is required: `y ~ Intercept(1) + x1 + x2`. Standard
#' `y ~ x1 + x2` formulas are not supported because inlabru does not add an
#' intercept automatically.
#'
#' IID random intercepts are supported in the fit, but `predict()` via the
#' parsnip interface returns the **marginal** mean — only the fixed effects
#' are included in the response-scale linear predictor. The IID random
#' intercept is integrated over rather than conditioned on any specific
#' group level, which matches the "new group" use case but silently ignores
#' the random effect for held-out observations from a seen group. If you
#' need conditional predictions, call `inlabru::predict()` directly with a
#' formula that references the IID component.
#'
#' @param mode Model mode. Currently only `"regression"` is supported.
#' @param family INLA family string, e.g. `"gaussian"`, `"poisson"`.
#' @return A `bru_reg` model spec.
#' @export
bru_reg <- function(mode = "regression", family = "gaussian") {
  args <- list(family = rlang::enquo(family))
  parsnip::new_model_spec(
    "bru_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

# Wrapper called by parsnip instead of inlabru::bru() directly.
#
# dic / waic default to TRUE so the fit object carries the model-fit criteria
# that glance() and the vetiver bridge expect. Override via
#   set_engine("inlabru", dic = FALSE, waic = FALSE)
# for large fits where WAIC is too expensive. config = TRUE is injected
# unconditionally — vetiver's serving handler needs stored configs for
# posterior sampling, so even a user-supplied `options` gets config = TRUE
# merged in.
#' Internal fit wrapper for the parsnip inlabru engine
#' @keywords internal
#' @export
bru_inlaops_fit <- function(
  formula,
  data,
  dic = TRUE,
  waic = TRUE,
  options = NULL,
  ...
) {
  .check_bru_intercept(formula)
  .check_bru_factors(formula, data)
  if (is.null(options)) {
    options <- inlabru::bru_options(
      control.compute = list(config = TRUE, dic = dic, waic = waic)
    )
  } else {
    cc <- options$control.compute
    if (is.null(cc)) {
      cc <- list()
    }
    if (!isTRUE(cc$config)) {
      cc$config <- TRUE
    }
    cc$dic <- cc$dic %||% dic
    cc$waic <- cc$waic %||% waic
    options$control.compute <- cc
  }
  inlabru::bru(formula, data = data, options = options, ...)
}

# Family → (default link name, inverse-link R function name). bru_pred_formula
# builds its response-scale formula around this table. Families not listed here
# are rejected by .bru_inverse_link; users who want a response-scale prediction
# for an unsupported family should call inlabru::predict() directly.
#
# Only families whose inverse link of the linear predictor is the response
# mean belong here. Excluded on purpose: zero-inflated families (the mean
# also depends on the zero-inflation probability), exponential / Weibull
# (exp(eta) is a rate, not a mean) and log-normal (exp(eta) is the median).
.bru_family_links <- list(
  gaussian = c(link = "identity", inv = "identity"),
  poisson = c(link = "log", inv = "exp"),
  nbinomial = c(link = "log", inv = "exp"),
  nbinomial2 = c(link = "log", inv = "exp"),
  gamma = c(link = "log", inv = "exp"),
  binomial = c(link = "logit", inv = "plogis"),
  betabinomial = c(link = "logit", inv = "plogis"),
  beta = c(link = "logit", inv = "plogis")
)

.bru_inverse_link <- function(family) {
  entry <- .bru_family_links[[family]]
  if (is.null(entry)) {
    stop(
      "bru_reg: no known default inverse link for family '",
      family,
      "'. ",
      "Call inlabru::predict() directly with an explicit response-scale ",
      "formula.",
      call. = FALSE
    )
  }
  unname(entry["inv"])
}

# Error if the fit overrode the family default link. .bru_inverse_link maps
# family → inverse link; if the user passed a non-default link via
# control.family$link or control.family$control.link$model, the hardcoded
# mapping is silently wrong, so bail.
.check_bru_default_link <- function(fit) {
  lhood <- inlabru::as_bru_obs_list(fit)[[1]]
  fam <- lhood$family %||% "gaussian"
  user_link <- lhood$control.family$link %||%
    lhood$control.family$control.link$model
  if (is.null(user_link) || identical(as.character(user_link), "default")) {
    return(invisible())
  }
  entry <- .bru_family_links[[fam]]
  if (is.null(entry)) {
    return(invisible())
  } # unknown family handled elsewhere
  default_link <- unname(entry["link"])
  if (!identical(as.character(user_link), default_link)) {
    stop(
      "bru_reg's predict() assumes the default link '",
      default_link,
      "' for family '",
      fam,
      "', but this fit specified link = '",
      as.character(user_link),
      "'. Call inlabru::predict() directly with ",
      "an explicit formula on the response scale.",
      call. = FALSE
    )
  }
  invisible()
}

#' Build the response-scale predict formula for the parsnip bru_reg engine
#'
#' The fixed-effect linear predictor wrapped in the default inverse link for
#' the fit's family. IID random intercepts (and any other random effects)
#' are intentionally excluded: the formula is built from `summary.fixed`
#' row names, which are the fixed-effect terms only. This produces the
#' marginal mean prediction — see the `bru_reg()` docs for details.
#' Exported only so that parsnip's quoted `args$formula` in `set_pred` can
#' reference it without `:::`. Not intended for end users.
#'
#' Errors if the fit specified a non-default link (via
#' `control.family = list(link = ...)`) or if the family has no known default
#' inverse link in the package's mapping. In either case, use
#' `inlabru::predict()` directly.
#'
#' @param fit A fitted `bru` object.
#' @return A one-sided formula.
#' @keywords internal
#' @export
bru_pred_formula <- function(fit) {
  .check_bru_default_link(fit)
  .warn_bru_random_effects(fit)
  fam <- inlabru::as_bru_obs_list(fit)[[1]]$family %||% "gaussian"
  inv <- .bru_inverse_link(fam)
  terms <- paste(rownames(fit$summary.fixed), collapse = " + ")
  if (identical(inv, "identity")) {
    stats::as.formula(paste0("~ ", terms))
  } else {
    stats::as.formula(paste0("~ ", inv, "(", terms, ")"))
  }
}

# Warn (once per R session) when the model has components beyond fixed
# effects. bru_pred_formula builds from summary.fixed only, so any IID /
# SPDE / BYM2 / AR1 / RW term is silently marginalised out of parsnip's
# response-scale prediction. Tell the user so they can switch to
# inlabru::predict() for conditional predictions.
.warn_bru_random_effects <- function(fit) {
  # summary.random is a named list of data frames, one per component.
  re_names <- names(fit$summary.random)
  if (is.null(re_names) || length(re_names) == 0L) {
    return(invisible())
  }
  rlang::warn(
    c(
      "This bru fit has non-fixed components that parsnip's predict() cannot condition on.",
      i = paste0("Dropped components: ", paste(re_names, collapse = ", ")),
      i = "`.pred` / `.pred_lower` / `.pred_upper` reflect only the fixed-effect linear predictor.",
      i = "For conditional predictions call inlabru::predict() directly."
    ),
    .frequency = "once",
    .frequency_id = paste0("inlaops-re-", paste(re_names, collapse = ","))
  )
  invisible()
}

# Shared intercept check used by bru_inlaops_fit and bru_cv_predict.
.check_bru_intercept <- function(formula) {
  terms_chr <- attr(stats::terms(formula), "term.labels")
  if (!any(grepl("^Intercept\\(", terms_chr))) {
    stop(
      "inlabru requires an explicit intercept component.\n",
      "Change your formula from `y ~ x1 + x2` to ",
      "`y ~ Intercept(1) + x1 + x2`.",
      call. = FALSE
    )
  }
  invisible()
}

# Factor and character predictors are not auto-encoded by bru_reg
# (predictor_indicators = "none" in set_encoding); inlabru errors obscurely
# when either is passed to a linear component. Catch both early with a
# message pointing at the three reasonable workarounds. Inputs to other
# component models (iid, factor_full, ...) are left alone: inlabru maps
# factor and character values to levels for those.
.check_bru_factors <- function(formula, data) {
  rhs_vars <- unique(unlist(lapply(
    .bru_rhs_terms(formula[[3L]]),
    .bru_linear_inputs
  )))
  vars_in_data <- intersect(rhs_vars, names(data))
  bad_vars <- vars_in_data[
    vapply(
      data[vars_in_data],
      function(col) is.factor(col) || is.character(col),
      logical(1)
    )
  ]
  if (length(bad_vars)) {
    stop(
      "bru_reg does not auto-encode factor or character predictors. ",
      "The following columns in `data` are factor / character: ",
      paste(shQuote(bad_vars), collapse = ", "),
      ".\n",
      "Options:\n",
      "  * Pre-encode with `recipes::step_dummy()` and `recipes::bake()` ",
      "before calling `fit()` (recipes/workflows cannot be wrapped around ",
      "`bru_reg` because they rebuild formulas without inlabru component ",
      "syntax).\n",
      "  * Convert to integer indicator columns in `data` before fitting.\n",
      "  * Use a factor component, e.g. ",
      "`habitat(habitat, model = \"factor_full\")`. Like other non-linear ",
      "components, it is left out of parsnip's predict().",
      call. = FALSE
    )
  }
  invisible()
}

# Split a component-syntax RHS on `+` into its terms.
.bru_rhs_terms <- function(expr) {
  if (
    is.call(expr) && identical(expr[[1L]], as.name("+")) && length(expr) == 3L
  ) {
    c(.bru_rhs_terms(expr[[2L]]), list(expr[[3L]]))
  } else {
    list(expr)
  }
}

# Variables feeding a linear component: a bare `x`, or `name(input)` with no
# model or model = "linear" (inlabru's default).
.bru_linear_inputs <- function(term) {
  if (is.name(term)) {
    return(as.character(term))
  }
  if (!is.call(term) || length(term) < 2L) {
    return(character())
  }
  model <- term$model
  if (!is.null(model) && !identical(model, "linear")) {
    return(character())
  }
  all.vars(term[[2L]])
}

#' Internal conf_int helper for the parsnip bru_reg engine
#'
#' Computes posterior credible intervals at an arbitrary level by sampling
#' via `inlabru::generate()` and taking the requested quantiles. Called by
#' parsnip's `predict(type = "conf_int", level = ...)` dispatch. Exported
#' only so `parsnip::set_pred`'s quoted `func` can reach it via the
#' `pkg = "inlaops"` lookup; not intended for end users.
#'
#' @param object A fitted `bru` object.
#' @param newdata A data frame of covariate values.
#' @param level Credible-interval level between 0 and 1.
#' @param formula Response-scale prediction formula.
#' @param n.samples Number of posterior samples (default 1000).
#' @return A list with `q_lower` and `q_upper` numeric vectors.
#' @keywords internal
#' @export
bru_predict_conf_int <- function(
  object,
  newdata,
  level,
  formula,
  n.samples = 1000L
) {
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      level <= 0 ||
      level >= 1
  ) {
    stop(
      "`level` must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }
  alpha <- (1 - level) / 2
  samples <- inlabru::generate(
    object,
    newdata = newdata,
    formula = formula,
    n.samples = n.samples
  )
  if (!is.matrix(samples) || nrow(samples) != nrow(newdata)) {
    stop(
      "inlabru::generate() did not return an N_obs x N_samples matrix; ",
      "cannot compute per-row quantiles. Call inlabru::predict() directly.",
      call. = FALSE
    )
  }
  list(
    q_lower = unname(apply(
      samples,
      1L,
      stats::quantile,
      probs = alpha,
      na.rm = TRUE
    )),
    q_upper = unname(apply(
      samples,
      1L,
      stats::quantile,
      probs = 1 - alpha,
      na.rm = TRUE
    ))
  )
}

# Called from .onLoad() — registers the bru_reg spec with parsnip.
#
# parsnip::set_new_model() errors if the model is already in the registry,
# which trips up devtools::load_all() / devtools::document() when run twice
# in the same R session (each rerun invokes .onLoad again). Bail early if
# parsnip already has bru_reg so that re-loading is idempotent.
make_bru_reg <- function() {
  registered <- tryCatch(
    "bru_reg" %in% parsnip::get_model_env()$models,
    error = function(e) FALSE
  )
  if (registered) {
    return(invisible())
  }

  parsnip::set_new_model("bru_reg")
  parsnip::set_model_mode("bru_reg", "regression")
  parsnip::set_model_engine("bru_reg", mode = "regression", eng = "inlabru")
  parsnip::set_dependency("bru_reg", eng = "inlabru", pkg = "inlabru")

  parsnip::set_model_arg(
    model = "bru_reg",
    eng = "inlabru",
    parsnip = "family",
    original = "family",
    func = list(pkg = "inlabru", fun = "bru"),
    has_submodel = FALSE
  )

  parsnip::set_encoding(
    model = "bru_reg",
    eng = "inlabru",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "bru_reg",
    eng = "inlabru",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "inlaops", fun = "bru_inlaops_fit"),
      # dic/waic defaults live on bru_inlaops_fit so users can override via
      # set_engine("inlabru", dic = FALSE, waic = FALSE).
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "bru_reg",
    eng = "inlabru",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(result, object) tibble::tibble(.pred = result$mean),
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        formula = quote(inlaops::bru_pred_formula(object$fit))
      )
    )
  )

  # conf_int: posterior credible interval at the user-requested `level`.
  # parsnip substitutes `level` into the quoted args, and the helper samples
  # via inlabru::generate() and takes the matching quantiles — so users get
  # the interval they asked for, not a hardcoded 95%.
  parsnip::set_pred(
    model = "bru_reg",
    eng = "inlabru",
    mode = "regression",
    type = "conf_int",
    value = list(
      pre = NULL,
      post = function(result, object) {
        tibble::tibble(
          .pred_lower = result$q_lower,
          .pred_upper = result$q_upper
        )
      },
      func = c(pkg = "inlaops", fun = "bru_predict_conf_int"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        level = quote(level),
        formula = quote(inlaops::bru_pred_formula(object$fit))
      )
    )
  )
}
