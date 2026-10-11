test_that("bru_cv_predict returns correct structure", {
  skip_if_not_installed("inlabru")
  skip_if_not_installed("rsample")
  skip_on_cran()

  library(inlabru)
  set.seed(1)
  n <- 60
  dat <- data.frame(x = rnorm(n))
  dat$y <- 1 + 0.5 * dat$x + rnorm(n, sd = 0.5)

  result <- bru_cv_predict(
    formula = y ~ Intercept(1) + x,
    family = "gaussian",
    data = dat,
    v = 3L,
    pred_formula = ~ Intercept + x,
    n_samples = 100L,
    seed = 42L
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(".truth", ".pred", ".pred_low", ".pred_high", ".pred_sd")
  )
  expect_equal(nrow(result), n)
  expect_true(all(is.finite(result$.pred)))
  expect_true(all(result$.pred_low < result$.pred))
  expect_true(all(result$.pred < result$.pred_high))
  expect_true(all(result$.pred_sd > 0))
})

test_that("bru_cv_predict seed makes fold assignment reproducible", {
  skip_if_not_installed("inlabru")
  skip_if_not_installed("rsample")
  skip_on_cran()

  library(inlabru)
  set.seed(99)
  n <- 40
  dat <- data.frame(x = rnorm(n))
  dat$y <- 1 + 0.5 * dat$x + rnorm(n, sd = 0.5)

  # .truth is determined solely by fold assignment, not by the INLA sampler,
  # so it must be identical across two calls with the same seed.
  r1 <- bru_cv_predict(
    y ~ Intercept(1) + x,
    "gaussian",
    dat,
    v = 2L,
    ~ Intercept + x,
    n_samples = 50L,
    seed = 7L
  )
  r2 <- bru_cv_predict(
    y ~ Intercept(1) + x,
    "gaussian",
    dat,
    v = 2L,
    ~ Intercept + x,
    n_samples = 50L,
    seed = 7L
  )

  expect_equal(r1$.truth, r2$.truth)
})

test_that("bru_cv_predict errors early when formula lacks Intercept()", {
  skip_if_not_installed("rsample")

  dat <- data.frame(x = rnorm(20), y = rnorm(20))
  expect_error(
    bru_cv_predict(
      y ~ x,
      "gaussian",
      dat,
      v = 2L,
      ~ Intercept + x,
      n_samples = 50L,
      seed = 1L
    ),
    "Intercept"
  )
})

test_that("bru_cv_predict errors without rsample", {
  skip_if(requireNamespace("rsample", quietly = TRUE), "rsample is installed")
  expect_error(
    bru_cv_predict(
      y ~ x,
      "gaussian",
      data.frame(y = 1, x = 1),
      v = 2L,
      ~x,
      seed = NULL
    ),
    "rsample"
  )
})

test_that("bru_cv_predict forwards ... to bru", {
  skip_if_not_installed("inlabru")
  skip_if_not_installed("rsample")
  skip_on_cran()

  library(inlabru)
  set.seed(17)
  n <- 30
  dat <- data.frame(x = rnorm(n))
  dat$y <- 1 + 0.5 * dat$x + rnorm(n, sd = 0.5)

  # Forward a custom options bundle; config = TRUE is injected by
  # bru_inlaops_fit so the caller only needs to set dic/waic here.
  result <- bru_cv_predict(
    formula = y ~ Intercept(1) + x,
    family = "gaussian",
    data = dat,
    v = 2L,
    pred_formula = ~ Intercept + x,
    n_samples = 50L,
    seed = 4L,
    options = bru_options(
      control.compute = list(dic = FALSE, waic = FALSE)
    )
  )

  expect_equal(nrow(result), n)
  expect_true(all(is.finite(result$.pred)))
})
