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
#' @param seed Optional integer. If non-`NULL`, the session RNG is reset to
#'   this value for the duration of the call via save/restore, so the
#'   caller's RNG stream is not disturbed.
#' @return A tibble with one row per observation and columns `crps`, `logs`,
#'   `se`, `ds`.
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

  samples <- .with_seed(
    seed,
    inlabru::generate(
      fit,
      newdata = newdata,
      formula = pred_formula,
      n.samples = n_samples
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
