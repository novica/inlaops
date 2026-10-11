skip_if_not_installed("inlabru")
skip_on_cran()

library(inlabru)

# ── shared fixtures ────────────────────────────────────────────────────────────

set.seed(42)
n <- 100
dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
dat$y <- 2 + 0.8 * dat$x1 - 0.4 * dat$x2 + rnorm(n, sd = 0.5)

idx <- sample(n, 20)
holdout <- dat[idx, ]
train <- dat[-idx, ]

fit <- bru(
  y ~ Intercept(1) + x1 + x2,
  family = "gaussian",
  data = train,
  options = bru_options(
    control.compute = list(config = TRUE, dic = TRUE, waic = TRUE)
  )
)

# ── tidy ───────────────────────────────────────────────────────────────────────

test_that("tidy_bru returns fixed effects with correct structure", {
  result <- tidy(fit)
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("term", "estimate", "std.error", "conf.low", "conf.high")
  )
  expect_equal(nrow(result), 3L)
  expect_equal(result$term, c("Intercept", "x1", "x2"))
  expect_true(all(result$conf.low < result$estimate))
  expect_true(all(result$estimate < result$conf.high))
})

test_that("tidy_bru returns hyperparameters", {
  result <- tidy(fit, effects = "hyperpar")
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("term", "estimate", "std.error", "conf.low", "conf.high")
  )
  expect_gte(nrow(result), 1L)
  expect_true(all(result$conf.low < result$estimate))
  expect_true(all(result$estimate < result$conf.high))
})

test_that("tidy_bru estimates are near true values", {
  result <- tidy(fit)
  expect_equal(result$estimate[result$term == "Intercept"], 2, tolerance = 0.2)
  expect_equal(result$estimate[result$term == "x1"], 0.8, tolerance = 0.2)
  expect_equal(result$estimate[result$term == "x2"], -0.4, tolerance = 0.2)
})

# ── glance ─────────────────────────────────────────────────────────────────────

test_that("glance_bru returns a one-row tibble with non-NA key fields", {
  result <- glance(fit)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_named(result, c("dic", "waic", "marginal_loglik", "nobs", "elapsed"))
  expect_false(is.na(result$dic))
  expect_false(is.na(result$waic))
  expect_false(is.na(result$marginal_loglik))
  expect_equal(result$nobs, nrow(train))
  expect_false(is.na(result$elapsed))
  expect_gt(result$elapsed, 0)
})

# ── augment ────────────────────────────────────────────────────────────────────

test_that("augment_bru adds fitted columns to data", {
  result <- augment(fit, data = holdout, pred_formula = ~ Intercept + x1 + x2)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(holdout))
  expect_true(all(
    c(".fitted", ".fitted_low", ".fitted_high", ".fitted_sd") %in% names(result)
  ))
  expect_true(all(result$.fitted_low < result$.fitted))
  expect_true(all(result$.fitted < result$.fitted_high))
  expect_true(all(result$.fitted_sd > 0))
})

# ── parsnip ────────────────────────────────────────────────────────────────────

test_that("bru_reg fits and predicts", {
  skip_if_not_installed("parsnip")

  fit_p <- bru_reg(family = "gaussian") |>
    parsnip::set_engine("inlabru") |>
    parsnip::fit(y ~ Intercept(1) + x1 + x2, data = train)

  expect_s3_class(fit_p, "model_fit")

  preds <- predict(fit_p, new_data = holdout)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), nrow(holdout))
  expect_true(all(is.finite(preds$.pred)))
})

test_that("bru_reg predicts on response scale for Poisson", {
  skip_if_not_installed("parsnip")

  set.seed(7)
  n <- 80
  pd <- data.frame(x = rnorm(n))
  pd$y <- rpois(n, exp(0.5 + 0.4 * pd$x))

  fit_p <- bru_reg(family = "poisson") |>
    parsnip::set_engine("inlabru") |>
    parsnip::fit(y ~ Intercept(1) + x, data = pd)

  preds <- predict(fit_p, new_data = pd[seq_len(10), ])
  # Response scale: predictions must be positive for Poisson. If bru_reg
  # silently returned the linear predictor (log-rate) for some rows the
  # sign would flip.
  expect_true(all(preds$.pred > 0))
})

test_that("bru_reg honours dic/waic opt-out via set_engine", {
  skip_if_not_installed("parsnip")

  fit_p <- bru_reg(family = "gaussian") |>
    parsnip::set_engine("inlabru", dic = FALSE, waic = FALSE) |>
    parsnip::fit(y ~ Intercept(1) + x1 + x2, data = train)

  expect_null(fit_p$fit$dic)
  expect_null(fit_p$fit$waic)
})

test_that("bru_reg exposes conf_int predict type", {
  skip_if_not_installed("parsnip")

  fit_p <- bru_reg(family = "gaussian") |>
    parsnip::set_engine("inlabru") |>
    parsnip::fit(y ~ Intercept(1) + x1 + x2, data = train)

  ci <- predict(fit_p, new_data = holdout, type = "conf_int")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(ci), nrow(holdout))
  expect_true(all(ci$.pred_lower < ci$.pred_upper))
})

test_that("bru_reg surfaces .check_bru_factors error at fit time", {
  skip_if_not_installed("parsnip")

  pd <- train
  pd$grp <- factor(sample(letters[1:3], nrow(pd), replace = TRUE))

  expect_error(
    bru_reg(family = "gaussian") |>
      parsnip::set_engine("inlabru") |>
      parsnip::fit(y ~ Intercept(1) + x1 + grp, data = pd),
    "factor"
  )
})

# ── bru_score ─────────────────────────────────────────────────────────────────

test_that("bru_score returns one row per holdout observation", {
  skip_if_not_installed("scoringRules")

  # pred_formula must produce Y samples including observation noise. Inside
  # an inlabru generate() formula the predictor is evaluated once per
  # posterior sample: latent-field symbols (Intercept) are scalars, while
  # covariates (x1, x2) are vectors over observations. So length(x1) is the
  # observation count for the per-sample r*() call. SD is fixed at the true
  # simulation value to keep the test deterministic across INLA versions
  # (the Gaussian-precision hyperparameter symbol name varies).
  y_formula <- ~ rnorm(
    length(x1),
    mean = Intercept + x1 + x2,
    sd = 0.5
  )
  result <- bru_score(fit, holdout, y_formula, "y", n_samples = 200L)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(holdout))
  expect_named(result, c("crps", "logs", "se", "ds"))
  expect_true(all(is.finite(result$crps)))
  expect_true(all(is.finite(result$logs)))
  expect_true(all(result$crps > 0))
  expect_true(all(result$se >= 0))
})

test_that("bru_score detects better-specified model", {
  skip_if_not_installed("scoringRules")

  fit_reduced <- bru(
    y ~ Intercept(1) + x1,
    family = "gaussian",
    data = train,
    options = bru_options(control.compute = list(config = TRUE))
  )

  y_full <- ~ rnorm(
    length(x1),
    mean = Intercept + x1 + x2,
    sd = 0.5
  )
  y_reduced <- ~ rnorm(
    length(x1),
    mean = Intercept + x1,
    sd = 0.5
  )

  scores_full <- bru_score(fit, holdout, y_full, "y", n_samples = 200L)
  scores_reduced <- bru_score(
    fit_reduced,
    holdout,
    y_reduced,
    "y",
    n_samples = 200L
  )

  expect_lt(mean(scores_full$crps), mean(scores_reduced$crps))
})

# ── vetiver round-trip ─────────────────────────────────────────────────────────

test_that("vetiver predict handler end-to-end: compile + predict + response shape", {
  skip_if_not_installed("vetiver")

  v <- vetiver::vetiver_model(fit, "handler_test")
  v$metadata$user <- list(
    pred_formula = "~ Intercept + x1 + x2",
    n_samples = 100L
  )

  # .handler_predict_bru is called by plumber once at API setup; simulate
  # that and invoke the returned per-request closure with a mock request.
  handler <- .handler_predict_bru(v)
  expect_type(handler, "closure")

  # plumber hands the handler a parsed body via req$body.
  mock_req <- list(body = holdout[seq_len(5), c("x1", "x2")])
  resp <- handler(mock_req)

  expect_s3_class(resp, "tbl_df")
  expect_named(resp, c(".pred", ".pred_low", ".pred_high", ".pred_sd"))
  expect_equal(nrow(resp), 5L)
  expect_true(all(is.finite(resp$.pred)))
  expect_true(all(resp$.pred_low < resp$.pred))
  expect_true(all(resp$.pred < resp$.pred_high))
  expect_true(all(resp$.pred_sd > 0))
})

test_that("vetiver predict handler rejects non-formula pred_formula at setup", {
  skip_if_not_installed("vetiver")

  v <- vetiver::vetiver_model(fit, "handler_evil")
  v$metadata$user <- list(
    pred_formula = "system('rm -rf /')",
    n_samples = 100L
  )

  # Should fail at handler *setup*, not at the first request.
  expect_error(.handler_predict_bru(v), "formula expression")
})

test_that("vetiver handler round-trip works for sf spatial data", {
  skip_if_not_installed("vetiver")
  skip_if_not_installed("sf")

  # Fit a trivial model on an sf points object. We don't need SPDE here —
  # the goal is to exercise the spatial branch of the handler:
  # detect sf at pin time, strip geometry in the ptype, and rebuild the
  # sf object from a plain-JSON request body at predict time.
  sf_df <- data.frame(
    lon = runif(50, 10, 11),
    lat = runif(50, 59, 60),
    x1 = rnorm(50)
  )
  sf_df$y <- 1 + 0.5 * sf_df$x1 + rnorm(50, sd = 0.3)
  sf_train <- sf::st_as_sf(sf_df, coords = c("lon", "lat"), crs = 4326)

  fit_sf <- bru(
    y ~ Intercept(1) + x1,
    family = "gaussian",
    data = sf_train,
    options = bru_options(control.compute = list(config = TRUE))
  )

  v <- vetiver::vetiver_model(fit_sf, "handler_spatial")
  v$metadata$user <- list(
    pred_formula = "~ Intercept + x1",
    n_samples = 100L,
    coord_names = c("lon", "lat"),
    crs = 4326L
  )

  handler <- .handler_predict_bru(v)

  # plumber would deliver req$body as parsed JSON — a plain data frame with
  # `lon`, `lat`, and `x1` columns. The handler must reconstruct the sf
  # object internally before calling predict.bru.
  req_body <- data.frame(
    lon = c(10.5, 10.7),
    lat = c(59.5, 59.6),
    x1 = c(0.0, 1.0)
  )
  resp <- handler(list(body = req_body))

  expect_s3_class(resp, "tbl_df")
  expect_equal(nrow(resp), 2L)
  expect_named(resp, c(".pred", ".pred_low", ".pred_high", ".pred_sd"))
  expect_true(all(is.finite(resp$.pred)))
})

test_that("vetiver_model wraps bru fit and pins correctly", {
  skip_if_not_installed("vetiver")
  skip_if_not_installed("pins")

  v <- vetiver::vetiver_model(fit, "test_model")
  expect_s3_class(v, "vetiver_model")
  expect_match(v$description, "inlabru model")
  expect_match(v$description, "gaussian")

  v$metadata$user <- list(
    pred_formula = "~ Intercept + x1 + x2",
    n_samples = 100L
  )

  board <- pins::board_temp()
  vetiver::vetiver_pin_write(board, v)

  versions <- pins::pin_versions(board, "test_model")
  expect_equal(nrow(versions), 1L)
})

test_that("spatial sf vetiver model pins and round-trips through the handler", {
  skip_if_not_installed("vetiver")
  skip_if_not_installed("pins")
  skip_if_not_installed("sf")

  # Build an sf points model.
  set.seed(101)
  sf_df <- data.frame(
    lon = runif(40, 10, 11),
    lat = runif(40, 59, 60),
    x1 = rnorm(40)
  )
  sf_df$y <- 1 + 0.5 * sf_df$x1 + rnorm(40, sd = 0.3)
  sf_train <- sf::st_as_sf(sf_df, coords = c("lon", "lat"), crs = 4326)

  fit_sf <- bru(
    y ~ Intercept(1) + x1,
    family = "gaussian",
    data = sf_train,
    options = bru_options(control.compute = list(config = TRUE))
  )

  # Task 4 regression: when called on the bru fit's lhood data, the ptype
  # function must surface coord columns. Asserting against `.vetiver_ptype_bru`
  # directly keeps this independent of vetiver's storage layout (`v$ptype`
  # vs `v$prototype` has changed across vetiver versions). What matters is
  # that whatever vetiver stores comes from this function.
  spatial_class <- .bru_detect_spatial_class(
    fit_sf$bru_info$lhoods[[1]]$data
  )
  expect_true(
    identical(spatial_class, "sf_point") ||
      identical(spatial_class, "sp_point")
  )
  ptype <- .vetiver_ptype_bru(fit_sf)
  expect_true("x1" %in% names(ptype))
  # Coord column names depend on whether inlabru kept sf (X, Y) or converted
  # to sp (whatever the user named them). Either is fine — assert >= 2
  # numeric coord-like columns alongside the covariates.
  expect_gte(ncol(ptype), 3L)

  # The user must align coord_names with whatever the ptype produced.
  coord_cols <- setdiff(names(ptype), names(sf::st_drop_geometry(sf_train)))
  expect_length(coord_cols, 2L)

  v <- vetiver::vetiver_model(fit_sf, "sf_roundtrip")
  v$metadata$user <- list(
    pred_formula = "~ Intercept + x1",
    n_samples = 100L,
    coord_names = coord_cols,
    crs = 4326L
  )

  board <- pins::board_temp()
  vetiver::vetiver_pin_write(board, v)
  v_read <- vetiver::vetiver_pin_read(board, "sf_roundtrip")

  # After pin_read, metadata$user round-trips via YAML; the handler must
  # re-parse the formula string and rebuild the spatial object from the
  # JSON body using whichever coord_cols the ptype advertised.
  handler <- .handler_predict_bru(v_read)
  body <- data.frame(c(10.3, 10.7), c(59.3, 59.7), c(-0.5, 0.8))
  names(body) <- c(coord_cols, "x1")
  resp <- handler(list(body = body))

  expect_s3_class(resp, "tbl_df")
  expect_equal(nrow(resp), 2L)
  expect_named(resp, c(".pred", ".pred_low", ".pred_high", ".pred_sd"))
  expect_true(all(is.finite(resp$.pred)))
})

test_that("vetiver_prepare_model rejects polygon sf fits end-to-end", {
  skip_if_not_installed("vetiver")
  skip_if_not_installed("sf")

  # Task 5 regression: a polygon fit must fail at pin time, not silently
  # get reconstructed as points on each request.
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  )
  poly_sf <- sf::st_sf(y = 1.0, x1 = 0.0, geometry = poly)

  # Fit a minimal dummy model just to populate bru_info. We don't need
  # posterior sampling to work — we want to verify .vetiver_prepare_model_bru
  # rejects polygon geometry before the user pins.
  fake <- structure(
    list(
      misc = list(configs = list(dummy = TRUE)),
      bru_info = list(lhoods = list(list(data = poly_sf, family = "gaussian")))
    ),
    class = c("bru", "inla")
  )
  expect_error(
    .vetiver_prepare_model_bru(fake),
    "areal"
  )
})

test_that("parsnip fit can be served via vetiver after manual unwrap", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("vetiver")

  fit_p <- bru_reg(family = "gaussian") |>
    parsnip::set_engine("inlabru") |>
    parsnip::fit(y ~ Intercept(1) + x1 + x2, data = train)

  # The bridge does not auto-unwrap parsnip's model_fit — users pass
  # fit_p$fit to vetiver_model. The .bru S3 methods then dispatch
  # normally and the standard handler returns the 4-column response.
  v <- vetiver::vetiver_model(fit_p$fit, "parsnip_handler_test")
  v$metadata$user <- list(
    pred_formula = "~ Intercept + x1 + x2",
    n_samples = 100L
  )

  handler <- .handler_predict_bru(v)
  expect_type(handler, "closure")

  resp <- handler(list(body = holdout[seq_len(3), c("x1", "x2")]))
  expect_named(resp, c(".pred", ".pred_low", ".pred_high", ".pred_sd"))
  expect_equal(nrow(resp), 3L)
  expect_true(all(is.finite(resp$.pred)))
})
