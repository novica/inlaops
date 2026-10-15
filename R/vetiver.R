#' @importFrom stats predict
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang `%||%`
NULL

# ── vetiver S3 methods for class "bru" ────────────────────────────────────────
# These are named with a leading dot so devtools::load_all() does not
# auto-detect them as S3 methods (which would fail because vetiver is in
# Suggests, not Imports). They are registered via registerS3method() in
# .onLoad() in zzz.R.

.vetiver_create_description_bru <- function(model) {
  families <- vapply(
    inlabru::as_bru_obs_list(model),
    function(l) l$family %||% "unknown",
    character(1)
  )
  paste0("An inlabru model (", paste(families, collapse = ", "), " family)")
}

.vetiver_create_meta_bru <- function(model, metadata) {
  # inlaops must be listed: the handler_predict method that serves bru fits
  # lives here, so an API built from the pin (e.g. in Docker) has to load it.
  vetiver::vetiver_meta(
    metadata,
    required_pkgs = c("inlaops", "inlabru", "INLA")
  )
}

.vetiver_ptype_bru <- function(model, ...) {
  obs <- inlabru::as_bru_obs_list(model)[[1]]
  lhood_data <- obs$data
  if (is.null(lhood_data)) {
    stop(
      "Cannot extract input prototype from bru fit: no data found on the ",
      "likelihood object. Pass a custom prototype via the `save_prototype` ",
      "argument of vetiver_model()."
    )
  }
  spatial_class <- .bru_detect_spatial_class(lhood_data)
  # Polygon / areal fits are rejected at prepare time; defensive check here.
  if (isTRUE(grepl("_polygon$", spatial_class))) {
    stop(
      "Areal / polygon spatial fits are not supported by the vetiver bridge ",
      "yet. See vignette(\"spatial\") for the areal-lookup pattern.",
      call. = FALSE
    )
  }
  if (identical(spatial_class, "sf_point")) {
    # Strip geometry but surface coord columns as plain numerics so vetiver's
    # JSON validator keeps them for the handler's reconstruction step. sf's
    # st_coordinates() returns columns named X, Y (or X, Y, Z) — that is the
    # request body contract for sf fits.
    #
    # For LGCP ("cp") fits the stored data is the observed points stacked
    # with inlabru's integration points (mesh nodes with a zero Z and
    # internal .block / weight columns). Covariates in an LGCP are spatial
    # functions, so the location is the only request input.
    is_cp <- identical(obs$family, "cp")
    if (is_cp) {
      lhood_data <- sf::st_zm(lhood_data)
    }
    coords <- sf::st_coordinates(lhood_data)
    ptype <- if (is_cp) {
      tibble(.rows = nrow(coords))
    } else {
      as_tibble(sf::st_drop_geometry(lhood_data))
    }
    ptype <- .bru_add_coords(ptype, coords)
  } else if (identical(spatial_class, "sp_point")) {
    ptype <- as_tibble(lhood_data@data)
    coords <- sp::coordinates(lhood_data)
    ptype <- .bru_add_coords(ptype, coords)
  } else {
    ptype <- as_tibble(lhood_data)
  }
  # Requests carry predictors only; the response is what /predict returns.
  response <- if (length(obs$formula) == 3L) all.vars(obs$formula[[2L]])
  ptype[0, setdiff(names(ptype), response), drop = FALSE]
}

# Append coordinate columns to a prototype. A coordinate named like a data
# column (e.g. response `y` with coordinates x / y) would silently replace it.
.bru_add_coords <- function(ptype, coords) {
  clash <- intersect(colnames(coords), names(ptype))
  if (length(clash)) {
    stop(
      "Coordinate column(s) ",
      paste(shQuote(clash), collapse = ", "),
      " clash with data columns of the same name. Rename the data columns ",
      "before fitting so requests can carry both.",
      call. = FALSE
    )
  }
  for (nm in colnames(coords)) {
    ptype[[nm]] <- coords[, nm]
  }
  ptype
}

.vetiver_prepare_model_bru <- function(model) {
  # bru inherits from inla, so misc$configs is on the model object directly
  if (is.null(model$misc$configs)) {
    stop(
      "The inlabru model does not have stored configs for posterior sampling. ",
      "Re-fit with `options = bru_options(control.compute = list(config = TRUE))`."
    )
  }
  spatial_class <- .bru_detect_spatial_class(
    inlabru::as_bru_obs_list(model)[[1]]$data
  )
  if (isTRUE(grepl("_polygon$", spatial_class))) {
    stop(
      "Areal / polygon spatial fits cannot be served by this bridge yet. ",
      "The planned path is areal lookup: pin the polygon set separately, ",
      "carry an area ID in the request body, and join at handler time. ",
      "Until that lands, serve polygon models via a custom plumber API that ",
      "calls inlabru::predict() directly. See vignette(\"spatial\").",
      call. = FALSE
    )
  }
  # LGCP fits are served like any other spatial point fit: the user supplies
  # query locations and a pred_formula such as `~ exp(Intercept + field)`,
  # and the handler returns posterior intensity samples. The output columns
  # describe a posterior summary, not specifically Y, so the same /predict
  # contract works — it is just intensity rather than counts.
  model
}

.handler_startup_bru <- function(vetiver_model) {
  requireNamespace("inlabru", quietly = TRUE)
}

.handler_predict_bru <- function(vetiver_model, ...) {
  # Parse once at handler setup (plumber calls this outer function once when
  # the API is wired up). Malformed pred_formula then fails at API startup
  # instead of on the first request, and the inner closure does no per-request
  # parsing.
  user_meta <- vetiver_model$metadata$user
  pred_expr <- .parse_pred_formula(user_meta$pred_formula)
  # 200 is a production-reasonable default — enough for stable mean +
  # 95% CI, and ~5x cheaper per request than the old 1000. Pin a new
  # version with metadata$user$n_samples = 1000 if you need tighter tails.
  n_samples <- user_meta$n_samples %||% 200L
  model_obj <- vetiver_model$model
  # Detect the spatial data class directly from the fit. Stashing this in
  # metadata would be fragile: vetiver_meta() folds the writer's metadata
  # into the `user` slot, and users typically overwrite `metadata$user`
  # wholesale at pin time to set pred_formula / coord_names / crs, which
  # would erase anything the writer stored there.
  spatial_class <- .bru_detect_spatial_class(
    inlabru::as_bru_obs_list(model_obj)[[1]]$data
  )
  # NULL when the API was built with vetiver_api(check_prototype = FALSE).
  ptype <- vetiver_model$prototype

  function(req) {
    new_data <- req$body
    # Same request validation as vetiver's own handlers.
    if (!is.null(ptype)) {
      new_data <- vetiver::vetiver_type_convert(new_data, ptype)
      new_data <- hardhat::scream(new_data, ptype)
    }
    new_data <- as_tibble(new_data)

    # Reconstruct spatial object if the model was fit on spatial data.
    # The user must store coord_names and crs in metadata$user at pin time.
    if (!is.null(spatial_class)) {
      new_data <- .bru_reconstruct_spatial(new_data, spatial_class, user_meta)
    }

    preds <- predict(
      model_obj,
      newdata = new_data,
      formula = pred_expr,
      n.samples = n_samples
    )

    tibble(
      .pred = preds$mean,
      .pred_low = preds$q0.025,
      .pred_high = preds$q0.975,
      .pred_sd = preds$sd
    )
  }
}

# Parse the prediction formula stored in pin metadata.
#
# Trust model: whoever can write to the pins board is trusted. A formula's
# RHS is arbitrary R evaluated by inlabru at predict time; this function
# only checks the metadata entry exists, parses to a formula, and returns it.
# No security walking — a pins-board writer who can pin a model can also
# pin arbitrary code, so walking names would be defence theatre.
.parse_pred_formula <- function(x) {
  if (is.null(x)) {
    stop(
      "metadata$user$pred_formula is not set. Store a prediction formula ",
      "(as a character string, e.g. \"~ exp(Intercept + x)\") before ",
      "pinning the vetiver model.",
      call. = FALSE
    )
  }
  if (inherits(x, "formula")) {
    return(x)
  }
  if (!is.character(x) || length(x) != 1L) {
    stop(
      "metadata$user$pred_formula must be a formula or a single string.",
      call. = FALSE
    )
  }
  parsed <- tryCatch(
    str2lang(x),
    error = function(e) {
      stop(
        "metadata$user$pred_formula did not parse: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.call(parsed) || !identical(parsed[[1L]], quote(`~`))) {
    stop(
      "metadata$user$pred_formula must be a formula expression ",
      "(e.g. \"~ Intercept + x\"), not an arbitrary R expression.",
      call. = FALSE
    )
  }
  eval(parsed, envir = baseenv())
}

# Note: parsnip-wrapped bru fits (class c("_bru_reg", "model_fit")) are NOT
# auto-unwrapped by the bridge. Users who wrap via parsnip::fit() should
# extract the inner bru fit before calling vetiver_model:
#   v <- vetiver_model(fit_p$fit, "name")
# Registering bridge-side methods on every parsnip class would require
# replicating all six S3 dispatch points (description, meta, ptype,
# prepare_model, handler_startup, handler_predict). The unwrap is a
# one-liner for the user and keeps the bridge surface narrow.

# ── spatial helpers ────────────────────────────────────────────────────────────

# Detect spatial container class and geometry kind. Returns one of:
#   "sf_point", "sf_polygon", "sp_point", "sp_polygon", or NULL
# Point fits use coordinate reconstruction from the request body. Polygon
# fits are rejected at prepare time (areal-lookup support deferred).
.bru_detect_spatial_class <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      return("sf_point")
    }
    geo_types <- as.character(sf::st_geometry_type(data))
    if (all(geo_types %in% c("POINT", "MULTIPOINT"))) {
      return("sf_point")
    }
    if (all(geo_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      return("sf_polygon")
    }
    return("sf_point") # mixed or unusual — let downstream code error
  }
  if (inherits(data, "SpatialPointsDataFrame")) {
    return("sp_point")
  }
  if (inherits(data, "SpatialPolygonsDataFrame")) {
    return("sp_polygon")
  }
  NULL
}

.bru_reconstruct_spatial <- function(data, spatial_class, user_meta) {
  coord_names <- user_meta$coord_names
  crs <- user_meta$crs

  if (is.null(coord_names)) {
    stop(
      "Spatial model detected but `coord_names` not found in ",
      "metadata$user. Store the coordinate column names at pin time:\n",
      "  v$metadata$user$coord_names <- c('X', 'Y')   # sf::st_coordinates names\n",
      "  v$metadata$user$crs <- <EPSG code or WKT>"
    )
  }

  if (identical(spatial_class, "sf_point")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to serve sf inlabru models.")
    }
    return(sf::st_as_sf(data, coords = coord_names, crs = crs))
  }
  if (identical(spatial_class, "sp_point")) {
    if (!requireNamespace("sp", quietly = TRUE)) {
      stop("Package 'sp' is required to serve sp inlabru models.")
    }
    coords <- as.matrix(data[, coord_names])
    data <- data[, setdiff(names(data), coord_names), drop = FALSE]
    return(sp::SpatialPointsDataFrame(
      coords = coords,
      data = as.data.frame(data),
      proj4string = .sp_crs(crs)
    ))
  }
  stop(
    "Unsupported spatial_class '",
    spatial_class,
    "' in .bru_reconstruct_spatial. Polygon / areal reconstruction is not ",
    "implemented yet.",
    call. = FALSE
  )
}

# Build an sp::CRS from either an EPSG code (numeric or all-digit string) or
# a WKT string. Anything NA / NULL becomes an undefined CRS.
.sp_crs <- function(crs) {
  if (is.null(crs) || (length(crs) == 1L && is.na(crs))) {
    return(sp::CRS(NA_character_))
  }
  if (is.numeric(crs)) {
    return(sp::CRS(SRS_string = paste0("EPSG:", as.integer(crs))))
  }
  if (is.character(crs) && length(crs) == 1L) {
    if (grepl("^[0-9]+$", crs)) {
      return(sp::CRS(SRS_string = paste0("EPSG:", crs)))
    }
    return(sp::CRS(SRS_string = crs))
  }
  stop(
    "Unsupported `crs` value for sp reconstruction: ",
    toString(crs),
    call. = FALSE
  )
}
