test_that("bru_pred_formula wraps linear predictor in inverse link by family", {
  fake_fit <- function(family) {
    structure(
      list(
        bru_info      = list(lhoods = list(list(family = family))),
        summary.fixed = data.frame(
          row.names = c("Intercept", "x1", "x2"),
          mean      = c(0, 0, 0)
        )
      ),
      class = c("bru", "inla")
    )
  }

  # Compare the formula body via format() — stats::as.formula captures its
  # calling environment, so expect_identical would diff the .Environment
  # attribute instead of the thing we care about.
  expect_equal(format(bru_pred_formula(fake_fit("gaussian"))),  "~Intercept + x1 + x2")
  expect_equal(format(bru_pred_formula(fake_fit("poisson"))),   "~exp(Intercept + x1 + x2)")
  expect_equal(format(bru_pred_formula(fake_fit("nbinomial"))), "~exp(Intercept + x1 + x2)")
  expect_equal(format(bru_pred_formula(fake_fit("binomial"))),  "~plogis(Intercept + x1 + x2)")
  expect_equal(format(bru_pred_formula(fake_fit("gamma"))),     "~exp(Intercept + x1 + x2)")
})

test_that("bru_pred_formula errors for unknown families", {
  fake_fit <- structure(
    list(
      bru_info      = list(lhoods = list(list(family = "freshfromthelab"))),
      summary.fixed = data.frame(
        row.names = c("Intercept", "x"),
        mean      = c(0, 0)
      )
    ),
    class = c("bru", "inla")
  )

  expect_error(
    bru_pred_formula(fake_fit),
    "no known default inverse link"
  )
})

test_that("bru_pred_formula errors when fit overrode the family default link", {
  fake_fit <- structure(
    list(
      bru_info = list(lhoods = list(list(
        family = "poisson",
        control.family = list(link = "identity")
      ))),
      summary.fixed = data.frame(
        row.names = c("Intercept", "x"),
        mean      = c(0, 0)
      )
    ),
    class = c("bru", "inla")
  )
  expect_error(
    bru_pred_formula(fake_fit),
    "default link"
  )
})

test_that(".check_bru_intercept rejects formulas without Intercept()", {
  expect_error(inlaops:::.check_bru_intercept(y ~ x), "Intercept")
  expect_silent(inlaops:::.check_bru_intercept(y ~ Intercept(1) + x))
})

test_that(".check_bru_factors errors for factor predictors with a helpful message", {
  dat <- data.frame(
    y      = 1:4,
    x      = c(1, 2, 3, 4),
    group  = factor(c("a", "b", "a", "b"))
  )
  expect_error(
    inlaops:::.check_bru_factors(y ~ Intercept(1) + x + group, dat),
    "factor"
  )
  expect_error(
    inlaops:::.check_bru_factors(y ~ Intercept(1) + x + group, dat),
    "recipes::step_dummy"
  )
})

test_that(".check_bru_factors passes when predictors are numeric", {
  dat <- data.frame(y = 1:4, x = c(1, 2, 3, 4), z = c(0L, 1L, 0L, 1L))
  expect_silent(inlaops:::.check_bru_factors(y ~ Intercept(1) + x + z, dat))
})

test_that(".check_bru_factors errors for character predictors", {
  dat <- data.frame(
    y     = 1:4,
    x     = c(1, 2, 3, 4),
    group = c("a", "b", "a", "b"),  # character, not factor
    stringsAsFactors = FALSE
  )
  expect_error(
    inlaops:::.check_bru_factors(y ~ Intercept(1) + x + group, dat),
    "character"
  )
})
