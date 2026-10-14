#' Cross-validation predictions for fixed-effects inlabru models
#'
#' Wraps `rsample::vfold_cv()` and re-fits the model on each training fold,
#' returning a tidy tibble of holdout predictions. Scoped to models with fixed
#' effects and IID random intercepts only.
#'
#' Structured random effects (SPDE, BYM2, AR1, RW1/RW2) cannot be
#' cross-validated by simply subsetting the data. Each requires per-fold
#' reconstruction of the index structure: SPDE models need a new A-matrix
#' (mesh projector) built from the fold's observation coordinates; BYM2 areal
#' models need the adjacency graph to account for areas with no fold
#' observations; AR1/RW models assume a contiguous integer time index that
#' breaks when folds remove time points. None of this can be done generically.
#' For those models use spatial block cross-validation and evaluate with
#' [bru_score()].
#'
#' Folds run sequentially. Callers who want parallel folds should compose with
#' their own parallel backend (e.g. `future.apply::future_lapply`) around a
#' per-fold `inlabru::bru()` call — this package stays out of the daemon
#' lifecycle to keep the scope focused on bridging, not execution.
#'
#' @param formula Model formula passed to `inlabru::bru()`.
#' @param family INLA family string.
#' @param data Training data frame.
#' @param v Number of folds (default 5).
#' @param pred_formula Formula passed to `inlabru::predict()` for the response
#'   scale (e.g. `~ exp(Intercept + x)`).
#' @param n_samples Number of posterior samples per fold (default 500).
#' @param seed Integer seed passed to `set.seed()` before fold assignment so
#'   that two identical calls produce identical fold splits. The session
#'   RNG state is restored after fold assignment via `on.exit`. `NULL`
#'   (default) leaves the session RNG state unchanged.
#' @param strata Optional name of a column in `data` to use for stratified
#'   sampling. Passed through to [rsample::vfold_cv()]. Useful for sparse
#'   counts or imbalanced binomial outcomes where random folds can leave a
#'   fold with zero events.
#' @param ... Extra named arguments forwarded to every per-fold
#'   `inlabru::bru()` call (e.g. `options = inlabru::bru_options(...)`,
#'   alternative `control.compute` settings). `config = TRUE` is injected
#'   unconditionally so per-fold posterior sampling always works.
#' @return A tibble with columns `.truth`, `.pred`, `.pred_low`,
#'   `.pred_high`, `.pred_sd`.
#' @export
bru_cv_predict <- function(
  formula,
  family,
  data,
  v = 5L,
  pred_formula,
  n_samples = 500L,
  seed = NULL,
  strata = NULL,
  ...
) {
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' is required for bru_cv_predict().")
  }

  # Fail fast with a helpful message if the formula lacks Intercept(),
  # before any folds are fit.
  .check_bru_intercept(formula)

  # rsample::vfold_cv uses tidyselect for its `strata` arg. Passing it a
  # bare character vector triggers a deprecation warning — convert to a
  # symbol and unquote. Skip the arg entirely when strata is NULL.
  folds <- .with_seed(seed, {
    if (is.null(strata)) {
      rsample::vfold_cv(data, v = v)
    } else {
      rsample::vfold_cv(data, v = v, strata = !!rlang::sym(strata))
    }
  })

  bru_extra <- list(...)
  results <- lapply(folds$splits, function(split) {
    .bru_cv_fit_fold(
      split,
      formula,
      family,
      pred_formula,
      n_samples,
      bru_extra
    )
  })

  dplyr::bind_rows(results)
}

# Fit one CV fold and return a tibble of holdout predictions.
.bru_cv_fit_fold <- function(
  split,
  formula,
  family,
  pred_formula,
  n_samples,
  bru_extra = list()
) {
  train <- rsample::analysis(split)
  test <- rsample::assessment(split)

  # bru_inlaops_fit ensures config = TRUE survives whether or not the caller
  # supplies options, so we just forward bru_extra as-is.
  fit_args <- c(
    list(formula, data = train, family = family),
    bru_extra
  )
  fit <- do.call(inlaops::bru_inlaops_fit, fit_args)

  preds <- stats::predict(
    fit,
    newdata = test,
    formula = pred_formula,
    n.samples = n_samples
  )

  tibble::tibble(
    .truth = test[[all.vars(formula)[1]]],
    .pred = preds$mean,
    .pred_low = preds$q0.025,
    .pred_high = preds$q0.975,
    .pred_sd = preds$sd
  )
}
