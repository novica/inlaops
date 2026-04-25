# inlaops 0.1.0 (2026-04-25)

Initial release. The package bridges inlabru fits to vetiver (versioning,
serving, monitoring, Docker) and to tidymodels (broom tidiers, a parsnip
engine for fixed-effects models, k-fold CV). Spatial point fits — `sf` /
`sp::SpatialPointsDataFrame` data, with or without an SPDE mesh — are
served as a first-class feature.

## vetiver bridge

* Six S3 methods registered against the `"bru"` class so
  `vetiver::vetiver_model()`, `vetiver::vetiver_pin_write()`, and
  `vetiver::vetiver_api()` work on inlabru fits out of the box.
* `/predict` endpoint returns posterior mean, 2.5th/97.5th quantiles, and
  posterior SD — not just a point estimate.
* **Spatial point fits are first-class.** The handler reconstructs `sf` /
  `sp` point objects from the JSON request body. Pin metadata carries
  `coord_names` (column names in the body) and `crs` (EPSG integer,
  digit string, or WKT). For sf fits the ptype surfaces coord columns
  using `sf::st_coordinates()` names — typically `X` and `Y`.
* **LGCP fits are served by the bridge.** The `/predict` endpoint returns
  posterior intensity samples when the user's `pred_formula` is on the
  intensity scale (`~ exp(Intercept + field)`). The wire columns are the
  standard `.pred / .pred_low / .pred_high / .pred_sd` posterior summary
  — for LGCP the values are intensity rather than counts. See
  `vignette("lgcp")`.
* **Polygon / areal fits are rejected at pin time.** Areal serving via ID
  lookup against a separately pinned polygon set is on the roadmap. The
  custom-plumber pattern in `vignette("lgcp")` (section 3) is the
  workaround in the meantime.
* `pred_formula` in pin metadata is parsed once at handler setup. There
  is no security walking of the RHS — a pins-board writer who can pin a
  model can also pin arbitrary code, so a blocklist would be defence
  theatre. Trust the board, not the formula.
* Default `n_samples` per request is 200 — production-reasonable. Pin a
  new model version with explicit `metadata$user$n_samples` for tighter
  tails.
* parsnip-wrapped fits are not auto-unwrapped by the bridge. Users who
  fit via `parsnip::fit()` should pass `fit_p$fit` to
  `vetiver_model()`.

## broom tidiers

* `tidy()` extracts fixed effects or hyperparameters from a `bru` fit
  into a five-column tibble (`term`, `estimate`, `std.error`, `conf.low`,
  `conf.high`).
* `glance()` returns a one-row tibble with DIC, WAIC, marginal
  log-likelihood, number of observations, and total wall-clock time.
  `nobs` is `NA` for any multi-likelihood fit — summing rows across
  likelihoods is ill-defined.
* `augment()` adds `.fitted`, `.fitted_low`, `.fitted_high`, and
  `.fitted_sd` columns to a data frame using the full posterior
  predictive distribution. Pre-existing `.fitted*` columns are dropped
  before bind, so `augment(augment(...))` does not duplicate. Optional
  `seed` argument is reproducible-with-restore: the caller's RNG state is
  not modified.

## parsnip constructor

* `bru_reg()` registers an `"inlabru"` engine for fixed-effects and IID
  random intercept models, enabling side-by-side comparison with
  frequentist GLMs in a tidymodels workflow.
* Formulas must use inlabru component syntax with an explicit intercept:
  `y ~ Intercept(1) + x1 + x2`. Factor and character predictors are
  rejected at fit time with a pointer to `recipes::step_dummy()` or
  inlabru's native factor components.
* `config = TRUE` is injected into the fit's options whether or not the
  user supplies their own `options` — a parsnip fit is always
  serving-ready for the vetiver bridge.
* `predict(type = "conf_int", level = ...)` honours the requested level
  by sampling via `inlabru::generate()` and taking matching quantiles.
* `bru_pred_formula()` errors if the fit specified a non-default link
  (e.g. `family = "poisson"` with `link = "identity"`) or a family
  outside the package's link mapping. These cases would silently produce
  wrong-scale predictions, so they fail loudly.
* Non-fixed components (IID, SPDE, BYM2, AR1, ...) trigger a
  once-per-session warning at predict time: parsnip predictions are
  marginal over them. Call `inlabru::predict()` directly for conditional
  predictions.

## predictive scoring

* `bru_score()` computes CRPS, log-score, squared error, and
  Dawid-Sebastiani score against held-out observations using
  `inlabru::generate()`.
* Thin wrapper over `scoringRules`: the user writes a `pred_formula` that
  produces Y samples including observation-model stochasticity. E.g. for
  Poisson: `~ rpois(length(x), exp(Intercept + x))`; for Gaussian:
  `~ rnorm(length(x), mean = Intercept + x, sd = known_sd)`. Inside a
  `generate()` formula the predictor is evaluated once per posterior
  sample — latent-field symbols are scalars, covariates are vectors over
  observations, so `length(<covariate>)` is the observation count.
* `seed` argument is reproducible-with-restore.

## cross-validation helper

* `bru_cv_predict()` wraps `rsample::vfold_cv()` for fixed-effects models
  and returns a tidy holdout prediction tibble. Spatial models with
  structured random effects (SPDE, BYM2, AR1, RW) are explicitly not
  supported — see `vignette("spatial")` for spatial block CV via
  `bru_score()`.
* Sequential only (no built-in parallelism). Callers who want parallel
  folds compose with their own backend (e.g. `future.apply`,
  `mirai::mirai_map`) around a per-fold `inlabru::bru()` call — keeps
  inlaops out of the daemon lifecycle.
* `strata` argument is forwarded to `rsample::vfold_cv()` for stratified
  sampling. `config = TRUE` is injected into per-fold options
  unconditionally.
