#' Predictive scores for inlabru models
#'
#' Thin wrapper over `scoringRules` that takes posterior samples from
#' `inlabru::generate()` and returns four predictive scores per observation.
#'
#' `pred_formula` must produce samples of `Y` — including observation-model
#' stochasticity for discrete families. The caller writes the full
#' distributional expression. Inside an inlabru `generate()` formula the
#' predictor is evaluated once per posterior sample: latent-field symbols
#' (`Intercept`, SPDE field values, ...) come in as scalars, and covariates
#' come in as vectors over observations. So `length(<covariate>)` gives
#' the observation count for the per-sample `r*()` call:
#'
#' * Poisson: `~ rpois(length(x), exp(Intercept + x))`
#' * Binomial: `~ rbinom(length(x), size = n_trials,
#'              prob = plogis(Intercept + x))`
#' * Gaussian: `~ rnorm(length(x), mean = Intercept + x,
#'              sd = 1 / sqrt(Precision_for_the_Gaussian_observations))`
#'
#' Hyperparameters are available in the formula under their standardised
#' names (see [inlabru::bru_standardise_names()]), so the Gaussian
#' observation noise is sampled jointly with the latent field.
#'
#' This is the same contract `inlabru::generate()` already exposes — the
#' package does not inject observation noise or dispatch on family. Users
#' who want response-scale samples for a family not listed above write the
#' appropriate random draw in the formula.
#'
#' CRPS and the kernel log-score are strictly proper for continuous Y.
#' For count / binomial Y the empirical CRPS on integer draws is still a
#' reasonable proper score; `scoringRules::logs_sample` uses a kernel-density
#' estimate which can be unstable on discrete supports — treat `logs` as
#' approximate in that case.
#'
#' Dawid-Sebastiani variance is floored at `.Machine$double.eps` to avoid
#' division by zero when the sample variance collapses.
#'
#' @param fit A fitted `bru` object. Must have been fit with
#'   `control.compute = list(config = TRUE)`.
#' @param newdata Data frame of covariate values for held-out observations.
#' @param pred_formula Formula passed to `inlabru::generate()`. Must produce
#'   draws of `Y` on the outcome scale; see Details.
#' @param y_col Name of the response column in `newdata`.
#' @param n_samples Number of posterior samples (default 1000).
#' @param seed Optional non-zero integer. If non-`NULL`, the scores are
#'   reproducible:
#'   the seed is used for R's RNG (the `r*()` draws in `pred_formula`) and
#'   for INLA's posterior sampler, which then runs single-threaded. The
#'   caller's RNG stream is restored afterwards.
#' @return A tibble with one row per observation and columns `crps`, `logs`,
#'   `se`, `ds`.
#' @references Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring
#'   rules, prediction, and estimation. *Journal of the American Statistical
#'   Association*, 102(477), 359--378. \doi{10.1198/016214506000001437}
#' @examplesIf interactive() && rlang::is_installed(c("INLA", "scoringRules", "sn"))
#' set.seed(1)
#' dat <- data.frame(x = rnorm(40))
#' dat$y <- rpois(40, exp(0.5 + 0.3 * dat$x))
#' fit <- bru_inlaops_fit(
#'   y ~ Intercept(1) + x,
#'   data = dat[1:30, ],
#'   family = "poisson"
#' )
#' bru_score(
#'   fit,
#'   newdata = dat[31:40, ],
#'   pred_formula = ~ rpois(length(x), exp(Intercept + x)),
#'   y_col = "y",
#'   n_samples = 200,
#'   seed = 1
#' )
#' @export
bru_score <- function(
  fit,
  newdata,
  pred_formula,
  y_col,
  n_samples = 1000L,
  seed = NULL
) {
  if (!requireNamespace("scoringRules", quietly = TRUE)) {
    stop("Package 'scoringRules' is required for bru_score().")
  }
  # Checked before sampling, which is the expensive part.
  if (
    !is.character(y_col) || length(y_col) != 1L || !y_col %in% names(newdata)
  ) {
    stop("`y_col` must name a single column of `newdata`.", call. = FALSE)
  }
  if (
    is.numeric(seed) && length(seed) == 1L && isTRUE(as.integer(seed) == 0L)
  ) {
    stop(
      "`seed` must be non-zero: INLA treats 0 as \"pick a random seed\".",
      call. = FALSE
    )
  }

  # Both seeds are needed: set.seed() covers R's RNG, `seed` covers INLA's
  # posterior sampler (0 lets INLA pick its own).
  samples <- .with_seed(
    seed,
    inlabru::generate(
      fit,
      newdata = newdata,
      formula = pred_formula,
      n.samples = n_samples,
      seed = if (is.null(seed)) 0L else as.integer(seed)
    )
  )

  if (!is.matrix(samples) || nrow(samples) != nrow(newdata)) {
    stop(
      "inlabru::generate() did not return an N_obs x N_samples matrix ",
      "(got ",
      paste(class(samples), collapse = "/"),
      ", nrow = ",
      NROW(samples),
      ", nrow(newdata) = ",
      nrow(newdata),
      "). ",
      "Use a pred_formula that produces one row per observation, or call ",
      "scoringRules directly on the generator output.",
      call. = FALSE
    )
  }

  y <- newdata[[y_col]]
  sample_mean <- rowMeans(samples)
  sample_var <- apply(samples, 1L, stats::var)
  var_floor <- pmax(sample_var, .Machine$double.eps)

  tibble::tibble(
    crps = scoringRules::crps_sample(y, samples),
    logs = scoringRules::logs_sample(y, samples),
    se = (y - sample_mean)^2,
    ds = (y - sample_mean)^2 / var_floor + log(var_floor)
  )
}
