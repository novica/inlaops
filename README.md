# inlaops

> **Status (0.1.0).** inlaops bridges
> [inlabru](https://inlabru-org.github.io/inlabru/) fits to vetiver and
> tidymodels. It versions, serves, and Dockerises Bayesian GLMs and
> SPDE point-geometry spatial models (`sf` / `sp::SpatialPointsDataFrame`),
> including LGCP intensity surfaces. Polygon / areal (BYM2) serving is
> rejected at pin time pending the areal-lookup design — fit those with
> inlabru as usual and use the custom-plumber pattern in `vignette("lgcp")`
> in the meantime. 0.1.0; not yet on CRAN.

[inlabru](https://inlabru-org.github.io/inlabru/) fits Bayesian spatial and
latent Gaussian models well. What it does not do is slot into the R deployment
and model-inspection tooling that the rest of the ecosystem takes for granted:
there is no path from a fitted `bru` object to a versioned REST API, no
`tidy()` or `glance()`, and no way to compare it against a frequentist baseline
in a shared workflow.

inlaops adds that plumbing. Concretely:

- **vetiver bridge**: six S3 methods that make `vetiver_model(bru_fit, "name")`
  work — versioning, REST API serving, and Docker packaging. Spatial point
  fits (`sf` / `sp::SpatialPointsDataFrame`, with or without an SPDE mesh)
  are first-class, including LGCP intensity surfaces. Polygon / areal
  models are rejected at pin time pending the areal-lookup design — see
  `vignette("lgcp")` for the custom-plumber fallback pattern.
- **broom tidiers**: `tidy()`, `glance()`, and `augment()` for `bru` objects,
  including SPDE hyperparameters and model-fit criteria (DIC, WAIC).
- **parsnip constructor**: `bru_reg()` for fixed-effects + IID random
  intercept inlabru models, enabling side-by-side comparison with frequentist
  GLMs in a tidymodels workflow. Spatial models with SPDE / BYM2 / structured
  random effects use inlabru's native fitting interface and the vetiver
  bridge for serving — they are not in scope for the parsnip layer.
- **predictive scoring**: `bru_score()` is a thin wrapper around
  `inlabru::generate()` and `scoringRules` that computes CRPS, log-score,
  squared error, and Dawid-Sebastiani against held-out observations.

The dependency chain is `inlaops → inlabru → INLA`. Because inlabru is on
CRAN, no non-standard repositories are needed to install inlaops itself.

---

## vetiver bridge

Six S3 methods registered on class `"bru"`. `vetiver_model()` works on inlabru
fits the same way it does on `lm()` or `mgcv::gam()`.

```r
library(inlabru)
library(vetiver)
library(pins)
library(inlaops)

# Fit. config = TRUE is required for posterior sampling at the endpoint —
# bru_inlaops_fit (used through parsnip) injects it for you, but a manual
# inlabru::bru() call needs it explicitly.
fit <- bru(
  y ~ Intercept(1) + x1 + x2,
  family  = "gaussian",
  data    = dat,
  options = bru_options(control.compute = list(config = TRUE,
                                               dic    = TRUE,
                                               waic   = TRUE))
)

# Wrap. vetiver_model() dispatches on class "bru" automatically.
# Pass pin metadata at construction so it is in place before the handler
# starts — pred_formula is required and must be a character string
# (survives YAML round-trip).
v <- vetiver_model(
  fit, "my_model",
  metadata = list(
    pred_formula = "~ Intercept + x1 + x2",
    n_samples    = 200L,
    waic         = fit$waic$waic,
    dic          = fit$dic$dic
  )
)

# Pin to a versioned board.
board <- board_temp("./model-store")
vetiver_pin_write(board, v)

# Serve locally.
library(plumber)
pr() |> vetiver_api(v) |> pr_run(port = 8088)

# Generate Docker artefacts (Dockerfile, plumber.R, renv.lock).
vetiver_prepare_docker(board, "my_model")
```

The `/predict` endpoint returns posterior mean, 2.5th/97.5th quantiles, and
posterior SD — not just a point estimate. The default of 200 posterior samples
per request is production-reasonable; pin a new model version with explicit
`metadata$user$n_samples` to widen the sampling.

### Spatial point fits

Spatial point fits are served by reconstructing the `sf` / `sp` object from
the JSON request body. Pin metadata must carry `coord_names` (the body's
coord column names) and `crs`:

```r
v$metadata$user <- list(
  pred_formula = "~ Intercept + field",
  n_samples    = 200L,
  coord_names  = c("X", "Y"),   # for sf fits, the names returned by
                                # sf::st_coordinates(fit_data) — typically X / Y
  crs          = 32632          # EPSG integer, EPSG-style digit string, or WKT
)
```

For sf fits the convention is `colnames(sf::st_coordinates(fit_data))`, which
is `X` / `Y` regardless of what you named the columns at construction. For sp
fits the user-chosen coord names are preserved. See `vignette("spatial")`.

**LGCP intensity surfaces** are served the same way as any other point
fit — pass `pred_formula = "~ exp(Intercept + field)"` and the `/predict`
endpoint returns posterior intensity samples at the query locations. See
`vignette("lgcp")`.

**Out of scope (rejected at pin time):**

- Polygon / areal geometry (`sf` polygon, `sp::SpatialPolygonsDataFrame`).
  Areal serving via ID lookup against a separately pinned polygon set is
  on the roadmap. Until then, fit via inlabru and use the custom-plumber
  pattern in `vignette("lgcp")` (section 3).

---

## tidymodels bridge

### broom tidiers

```r
library(broom)

tidy(fit)                        # fixed effects as a tibble
tidy(fit, effects = "hyperpar")  # hyperparameters (SPDE range, precision, ...)
glance(fit)                      # DIC, WAIC, marginal log-likelihood, nobs
augment(fit, data = dat, pred_formula = ~ Intercept + x1 + x2)  # fitted values
```

`glance()$nobs` is `NA` for any multi-likelihood fit — summing rows across
likelihoods is ill-defined.

### parsnip model constructor

Scoped to fixed-effects and IID random intercept models. For spatial models
with SPDE / BYM2 / structured random effects, fit via inlabru directly and
serve via the vetiver bridge above.

```r
library(parsnip)

spec <- bru_reg(mode = "regression", family = "poisson") |>
  set_engine("inlabru")

fit <- spec |> fit(counts ~ Intercept(1) + area + effort, data = dat)
predict(fit, new_data = new_dat)                                    # posterior mean
predict(fit, new_data = new_dat, type = "conf_int", level = 0.95)   # credible interval
```

`config = TRUE` is injected unconditionally so a parsnip fit is always
serving-ready for the vetiver bridge. `level` is honoured: passing `0.9`
returns 90% intervals, `0.99` returns 99%.

If you want to serve a parsnip-wrapped fit via vetiver, pass the inner bru
fit:

```r
v <- vetiver_model(fit$fit, "my_glm")
```

The bridge does not auto-unwrap parsnip's `model_fit`.

### Cross-validation

```r
cv_preds <- bru_cv_predict(
  formula      = counts ~ Intercept(1) + area + effort,
  family       = "poisson",
  data         = dat,
  v            = 5L,
  pred_formula = ~ exp(Intercept + area + effort),
  strata       = "area"  # optional, forwarded to rsample::vfold_cv
)
```

Sequential. Compose with `future.apply` or `mirai::mirai_map` around a
per-fold `inlabru::bru()` call if you need parallelism.

### Proper scoring rules

inlabru practitioners evaluate models against the full posterior predictive
distribution, not just the posterior mean. `bru_score()` is a thin wrapper
over `scoringRules`: pass a `pred_formula` that produces samples of `Y`,
including any observation-model stochasticity:

```r
# Poisson: write the rpois call yourself. length(<covariate>) is the
# observation count for the per-sample r*() call (latent-field symbols
# are scalars per sample inside an inlabru predictor formula).
scores <- bru_score(
  fit,
  newdata      = holdout_dat,
  pred_formula = ~ rpois(length(area), exp(Intercept + area + effort)),
  y_col        = "counts",
  n_samples    = 1000L
)

# One row per holdout observation
mean(scores$crps)
mean(scores$logs)
```

The package does not inject family-aware observation noise. For binomial,
write `rbinom(length(x), size = n_trials, prob = plogis(...))`; for nbinomial,
`rnbinom(...)`; for ZI families, the appropriate mixture. Same contract as
`inlabru::generate()`.

WAIC and DIC (via `glance()`) are computed on training data; CRPS and
log-score are computed on genuinely held-out observations. See
`vignette("scoring")` for details.

---

## Installation

```r
# inlaops is not yet on CRAN — install from GitHub
remotes::install_github("novica/inlaops")

# Core dependencies (all on CRAN)
install.packages(c("inlabru", "tibble", "dplyr", "generics", "rlang"))

# Optional: vetiver bridge
install.packages(c("vetiver", "pins", "plumber"))

# Optional: tidymodels bridge
install.packages(c("parsnip", "broom", "rsample", "scoringRules"))

# Optional: spatial fits
install.packages(c("sf", "sp"))
```

INLA is not on CRAN and is pulled in automatically through inlabru's
dependency declaration. If it is not already installed:

```r
install.packages("INLA",
  repos = c(INLA = "https://inla.r-inla-download.org/R/stable"),
  dep   = TRUE
)
```

For Docker images of spatial fits, use `rocker/geospatial` as the base —
`vetiver_prepare_docker()`'s default `rocker/r-ver` does not include
GDAL/PROJ/GEOS. See `vignette("docker")`.

---

## Vignettes

| Vignette | Content |
|---|---|
| `vignette("vetiver", package = "inlaops")` | Fitting, pinning, serving, and monitoring inlabru models with vetiver |
| `vignette("docker", package = "inlaops")` | Packaging a fitted model into a Docker container, including the geospatial base image for spatial fits |
| `vignette("glm", package = "inlaops")` | parsnip workflow for Bayesian GLMs; comparison with frequentist baselines |
| `vignette("spatial", package = "inlaops")` | Spatial fits: what the vetiver bridge serves, what is rejected at pin time, and the request-body contract |
| `vignette("scoring", package = "inlaops")` | Scoring rules for evaluating predictive distributions against held-out data |
| `vignette("lgcp", package = "inlaops")` | Serving LGCP intensity surfaces via the vetiver bridge, plus a custom-plumber fallback for non-default request shapes |
