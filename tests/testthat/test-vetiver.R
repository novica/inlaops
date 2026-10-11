test_that("vetiver_create_description produces correct string", {
  fake_fit <- structure(
    list(
      bru_info = list(
        lhoods = list(
          list(family = "gaussian"),
          list(family = "poisson")
        )
      )
    ),
    class = c("bru", "inla")
  )
  desc <- .vetiver_create_description_bru(fake_fit)
  expect_match(desc, "gaussian")
  expect_match(desc, "poisson")
  expect_match(desc, "inlabru model")
})

test_that("vetiver_ptype extracts zero-row tibble from first likelihood", {
  df <- data.frame(x1 = 1:3, x2 = c(1.1, 2.2, 3.3))
  fake_fit <- structure(
    list(bru_info = list(lhoods = list(list(data = df)))),
    class = c("bru", "inla")
  )
  ptype <- .vetiver_ptype_bru(fake_fit)
  expect_s3_class(ptype, "tbl_df")
  expect_equal(nrow(ptype), 0L)
  expect_named(ptype, c("x1", "x2"))
})

test_that("vetiver_ptype errors when no data on likelihood", {
  fake_fit <- structure(
    list(bru_info = list(lhoods = list(list(data = NULL)))),
    class = c("bru", "inla")
  )
  expect_error(.vetiver_ptype_bru(fake_fit), "no data found")
})

test_that("vetiver_prepare_model errors when configs absent", {
  fake_fit <- structure(
    list(misc = list(configs = NULL)),
    class = c("bru", "inla")
  )
  expect_error(.vetiver_prepare_model_bru(fake_fit), "config = TRUE")
})

test_that("vetiver_prepare_model passes through when configs present", {
  fake_fit <- structure(
    list(misc = list(configs = list(something = TRUE))),
    class = c("bru", "inla")
  )
  result <- .vetiver_prepare_model_bru(fake_fit)
  expect_identical(result, fake_fit)
})

# ── spatial round-trip ─────────────────────────────────────────────────────────

test_that("vetiver_ptype surfaces sf coord columns alongside attributes", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = 1:3, y = 1:3, val = 10:12),
    coords = c("x", "y"),
    crs = 4326
  )
  fake_fit <- structure(
    list(bru_info = list(lhoods = list(list(data = pts)))),
    class = c("bru", "inla")
  )
  ptype <- .vetiver_ptype_bru(fake_fit)
  expect_s3_class(ptype, "tbl_df")
  expect_equal(nrow(ptype), 0L)
  expect_true("val" %in% names(ptype))
  expect_false("geometry" %in% names(ptype))
  # Coord columns must be present so vetiver's JSON validator does not strip
  # them before .handler_predict_bru reconstructs the spatial object.
  expect_true(all(c("X", "Y") %in% names(ptype)))
})

test_that("vetiver_ptype surfaces sp coord columns alongside attributes", {
  skip_if_not_installed("sp")

  coords <- matrix(
    c(1, 2, 3, 4),
    ncol = 2,
    dimnames = list(NULL, c("lon", "lat"))
  )
  spdf <- sp::SpatialPointsDataFrame(coords, data.frame(val = c(10L, 20L)))
  fake_fit <- structure(
    list(bru_info = list(lhoods = list(list(data = spdf)))),
    class = c("bru", "inla")
  )
  ptype <- .vetiver_ptype_bru(fake_fit)
  expect_s3_class(ptype, "tbl_df")
  expect_equal(nrow(ptype), 0L)
  expect_true(all(c("val", "lon", "lat") %in% names(ptype)))
})

test_that("vetiver_prepare_model rejects polygon sf fits with a clear message", {
  skip_if_not_installed("sf")

  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  )
  poly_sf <- sf::st_sf(val = 1L, geometry = poly)
  fake_fit <- structure(
    list(
      misc = list(configs = list(dummy = TRUE)),
      bru_info = list(lhoods = list(list(data = poly_sf, family = "gaussian")))
    ),
    class = c("bru", "inla")
  )
  expect_error(
    .vetiver_prepare_model_bru(fake_fit),
    "areal"
  )
})

test_that(".bru_reconstruct_spatial rebuilds sf object correctly", {
  skip_if_not_installed("sf")

  df <- tibble::tibble(lon = c(10.0, 11.0), lat = c(59.0, 60.0), z = c(1L, 2L))
  result <- .bru_reconstruct_spatial(
    df,
    "sf_point",
    list(coord_names = c("lon", "lat"), crs = 4326L)
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2L)
  expect_true("z" %in% names(result))
  expect_false("lon" %in% names(result))
  expect_false("lat" %in% names(result))
  expect_equal(sf::st_crs(result)$epsg, 4326L)
})

test_that(".bru_reconstruct_spatial rebuilds SpatialPointsDataFrame correctly", {
  skip_if_not_installed("sp")

  df <- tibble::tibble(lon = c(10.0, 11.0), lat = c(59.0, 60.0), z = c(1L, 2L))
  result <- .bru_reconstruct_spatial(
    df,
    "sp_point",
    list(coord_names = c("lon", "lat"), crs = NULL)
  )

  expect_s4_class(result, "SpatialPointsDataFrame")
  expect_equal(nrow(result), 2L)
  expect_named(result@data, "z")
})

test_that(".bru_reconstruct_spatial errors for polygon classes", {
  df <- tibble::tibble(id = 1L)
  expect_error(
    .bru_reconstruct_spatial(
      df,
      "sf_polygon",
      list(coord_names = c("x", "y"))
    ),
    "not implemented"
  )
})

test_that(".sp_crs handles EPSG integers, EPSG strings, and WKT", {
  skip_if_not_installed("sp")

  expect_s4_class(.sp_crs(4326L), "CRS")
  expect_s4_class(.sp_crs("4326"), "CRS")
  expect_s4_class(.sp_crs(NULL), "CRS")
  expect_s4_class(.sp_crs(NA), "CRS")
  # Short WKT1 for WGS84. This should not get an "EPSG:" prefix glued onto it.
  wkt <- paste0(
    'GEOGCS["WGS 84",',
    'DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],',
    'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]]'
  )
  expect_s4_class(.sp_crs(wkt), "CRS")
})

test_that(".parse_pred_formula accepts formulas and formula strings", {
  expect_identical(
    .parse_pred_formula(~ Intercept + x),
    ~ Intercept + x
  )
  expect_s3_class(
    .parse_pred_formula("~ exp(Intercept + x)"),
    "formula"
  )
  expect_s3_class(
    .parse_pred_formula("~ plogis(Intercept + x1 + x2)"),
    "formula"
  )
  # Qualified calls and slot access are allowed — the trust model is that
  # whoever writes to the pins board is trusted.
  expect_s3_class(
    .parse_pred_formula("~ base::exp(Intercept + x)"),
    "formula"
  )
})

test_that(".parse_pred_formula rejects non-formula expressions", {
  expect_error(
    .parse_pred_formula("system('rm -rf /')"),
    "formula expression"
  )
  expect_error(
    .parse_pred_formula("1 + 1"),
    "formula expression"
  )
  expect_error(
    .parse_pred_formula(NULL),
    "not set"
  )
  expect_error(
    .parse_pred_formula(42),
    "must be a formula or a single string"
  )
})

test_that(".parse_pred_formula gives a helpful error on parse failure", {
  expect_error(
    .parse_pred_formula("~ x +"),
    "did not parse"
  )
})

test_that(".bru_reconstruct_spatial errors when coord_names missing", {
  df <- tibble::tibble(x = 1.0, y = 2.0)
  expect_error(
    .bru_reconstruct_spatial(df, "sf_point", list()),
    "coord_names"
  )
})
