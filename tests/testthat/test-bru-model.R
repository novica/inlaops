test_that("bru_pred_formula wraps linear predictor in inverse link by family", {
  fake_fit <- function(family) {
    structure(
      list(
        bru_info = mock_bru_info(list(list(family = family))),
        summary.fixed = data.frame(
          row.names = c("Intercept", "x1", "x2"),
          mean = c(0, 0, 0)
        )
      ),
      class = c("bru", "inla")
    )
  }

  # Compare the formula body via format() — stats::as.formula captures its
  # calling environment, so expect_identical would diff the .Environment
  # attribute instead of the thing we care about.
  expect_equal(
    format(bru_pred_formula(fake_fit("gaussian"))),
    "~Intercept + x1 + x2"
  )
  expect_equal(
    format(bru_pred_formula(fake_fit("poisson"))),
    "~exp(Intercept + x1 + x2)"
  )
  expect_equal(
    format(bru_pred_formula(fake_fit("nbinomial"))),
    "~exp(Intercept + x1 + x2)"
  )
  expect_equal(
    format(bru_pred_formula(fake_fit("binomial"))),
    "~plogis(Intercept + x1 + x2)"
  )
  expect_equal(
    format(bru_pred_formula(fake_fit("gamma"))),
    "~exp(Intercept + x1 + x2)"
  )
})

test_that("bru_pred_formula errors for unknown families", {
  fake_fit <- structure(
    list(
      bru_info = mock_bru_info(list(list(family = "freshfromthelab"))),
      summary.fixed = data.frame(
        row.names = c("Intercept", "x"),
        mean = c(0, 0)
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
      bru_info = mock_bru_info(list(list(
        family = "poisson",
        control.family = list(link = "identity")
      ))),
      summary.fixed = data.frame(
        row.names = c("Intercept", "x"),
        mean = c(0, 0)
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
  expect_error(.check_bru_intercept(y ~ x), "Intercept")
  expect_silent(.check_bru_intercept(y ~ Intercept(1) + x))
})

test_that(".check_bru_factors errors for factor predictors with a helpful message", {
  dat <- data.frame(
    y = 1:4,
    x = c(1, 2, 3, 4),
    group = factor(c("a", "b", "a", "b"))
  )
  expect_error(
    .check_bru_factors(y ~ Intercept(1) + x + group, dat),
    "factor"
  )
  expect_error(
    .check_bru_factors(y ~ Intercept(1) + x + group, dat),
    "recipes::step_dummy"
  )
})

test_that(".check_bru_factors passes when predictors are numeric", {
  dat <- data.frame(y = 1:4, x = c(1, 2, 3, 4), z = c(0L, 1L, 0L, 1L))
  expect_silent(.check_bru_factors(y ~ Intercept(1) + x + z, dat))
})

test_that(".check_bru_factors errors for character predictors", {
  dat <- data.frame(
    y = 1:4,
    x = c(1, 2, 3, 4),
    group = c("a", "b", "a", "b"), # character, not factor
    stringsAsFactors = FALSE
  )
  expect_error(
    .check_bru_factors(y ~ Intercept(1) + x + group, dat),
    "character"
  )
})

# A minimal bru-like fit for bru_pred_formula(): one observation model plus
# fixed and (optionally) random effects summaries.
fake_pred_fit <- function(obs, summary.random = NULL) {
  structure(
    list(
      bru_info = mock_bru_info(list(obs)),
      summary.fixed = data.frame(
        row.names = c("Intercept", "x"),
        mean = c(0, 0)
      ),
      summary.random = summary.random
    ),
    class = c("bru", "inla")
  )
}

test_that("bru_pred_formula rejects families whose inverse link is not the mean", {
  for (family in c(
    "zeroinflatedpoisson1",
    "exponential",
    "weibull",
    "lognormal"
  )) {
    expect_error(
      bru_pred_formula(fake_pred_fit(list(family = family))),
      "no known default inverse link"
    )
  }
})

test_that("bru_pred_formula errors on a link set via control.link", {
  fit <- fake_pred_fit(list(
    family = "poisson",
    control.family = list(control.link = list(model = "quantile"))
  ))
  expect_error(bru_pred_formula(fit), "default link")

  fit_default <- fake_pred_fit(list(
    family = "poisson",
    control.family = list(control.link = list(model = "default"))
  ))
  expect_no_error(bru_pred_formula(fit_default))
})

test_that("bru_pred_formula warns that random effects are left out", {
  fit <- fake_pred_fit(
    list(family = "gaussian"),
    summary.random = list(site = data.frame(ID = 1:2, mean = 0))
  )
  expect_warning(bru_pred_formula(fit), "Dropped components: site")
})

test_that(".check_bru_factors allows factor and character inputs to iid components", {
  dat <- data.frame(
    y = 1:4,
    x = c(1, 2, 3, 4),
    site = c("a", "b", "a", "b"),
    grp = factor(c("a", "b", "a", "b"))
  )
  expect_silent(.check_bru_factors(
    y ~ Intercept(1) + x + site(site, model = "iid") + g(grp, model = "iid"),
    dat
  ))
  # A linear component with a factor input is still caught.
  expect_error(
    .check_bru_factors(y ~ Intercept(1) + beta(grp, model = "linear"), dat),
    "factor"
  )
})

test_that("bru_inlaops_fit keeps dic/waic defaults when options are supplied", {
  skip_if_not_installed("inlabru")
  local_mocked_bindings(
    bru = function(formula, data, options, ...) options,
    .package = "inlabru"
  )
  dat <- data.frame(y = 1:3, x = c(0.1, 0.2, 0.3))

  opts <- bru_inlaops_fit(
    y ~ Intercept(1) + x,
    dat,
    options = list(control.compute = list(dic = FALSE))
  )

  expect_true(opts$control.compute$config)
  expect_false(opts$control.compute$dic) # user's choice wins
  expect_true(opts$control.compute$waic) # default fills the gap
})

test_that("bru_predict_conf_int validates level", {
  for (level in list(0, 1, -0.5, NA_real_, c(0.5, 0.9), "0.9")) {
    expect_error(
      bru_predict_conf_int(NULL, data.frame(), level, ~1),
      "strictly between 0 and 1"
    )
  }
})

test_that("make_bru_reg can run more than once", {
  skip_if_not_installed("parsnip")
  expect_no_error(make_bru_reg())
  expect_no_error(make_bru_reg())
})
