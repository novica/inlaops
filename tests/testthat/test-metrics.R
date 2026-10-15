# inlabru::generate() is mocked so these tests exercise bru_score() itself
# without a fitted model.
mock_generate <- function(samples, env = parent.frame()) {
  calls <- new.env()
  local_mocked_bindings(
    generate = function(...) {
      calls$args <- list(...)
      samples
    },
    .package = "inlabru",
    .env = env
  )
  calls
}

test_that("bru_score returns one row of scores per observation", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")

  set.seed(1)
  y <- c(0, 1, 2)
  mock_generate(matrix(rnorm(300, mean = rep(y, 100)), nrow = 3))

  scores <- bru_score(NULL, data.frame(y = y), ~1, y_col = "y")

  expect_s3_class(scores, "tbl_df")
  expect_named(scores, c("crps", "logs", "se", "ds"))
  expect_equal(nrow(scores), 3L)
  expect_true(all(is.finite(as.matrix(scores))))
})

test_that("bru_score ds stays finite when sample variance collapses", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")

  mock_generate(rbind(rep(1.0, 100), rep(2.0, 100)))

  scores <- bru_score(NULL, data.frame(y = c(1, 2)), ~1, y_col = "y")

  expect_true(all(is.finite(scores$ds)))
})

test_that("bru_score errors when generate() output has the wrong shape", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")

  mock_generate(matrix(0, nrow = 1, ncol = 10))

  expect_error(
    bru_score(NULL, data.frame(y = c(1, 2)), ~1, y_col = "y"),
    "N_obs x N_samples"
  )
})

test_that("bru_score checks y_col before sampling", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")

  calls <- mock_generate(matrix(0, nrow = 1, ncol = 10))

  expect_error(
    bru_score(NULL, data.frame(y = 1), ~1, y_col = "count"),
    "y_col"
  )
  expect_null(calls$args)
})

test_that("bru_score passes the seed to INLA's sampler", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")

  calls <- mock_generate(matrix(1, nrow = 1, ncol = 10))

  bru_score(NULL, data.frame(y = 1), ~1, y_col = "y", seed = 3L)
  expect_identical(calls$args$seed, 3L)

  bru_score(NULL, data.frame(y = 1), ~1, y_col = "y")
  expect_identical(calls$args$seed, 0L)
})

test_that("bru_score requires scoringRules", {
  # If scoringRules is available, we can't easily test the missing-package
  # branch; gate on it being absent.
  skip_if(requireNamespace("scoringRules", quietly = TRUE))
  expect_error(
    bru_score(
      fit = NULL,
      newdata = data.frame(),
      pred_formula = ~1,
      y_col = "y"
    ),
    "scoringRules"
  )
})

test_that("bru_score rejects non-integer seed", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")
  expect_error(
    bru_score(
      fit = NULL,
      newdata = data.frame(y = 1),
      pred_formula = ~1,
      y_col = "y",
      seed = "not-a-number"
    ),
    "single integer"
  )
})

test_that("bru_score rejects seed = 0, which INLA treats as random", {
  skip_if_not_installed("scoringRules")
  skip_if_not_installed("inlabru")
  calls <- mock_generate(matrix(1, nrow = 1, ncol = 10))

  expect_error(
    bru_score(NULL, data.frame(y = 1), ~1, y_col = "y", seed = 0),
    "non-zero"
  )
  expect_null(calls$args)
})
