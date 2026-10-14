# Run `expr` with a temporarily-set RNG seed and restore the caller's
# .Random.seed afterward. NULL `seed` is a no-op (caller's RNG flows through).
# Used by bru_score and bru_cv_predict to make `seed` arguments
# reproducible without leaking RNG state into the user's session.
.with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(eval.parent(substitute(expr)))
  }
  if (!is.numeric(seed) || length(seed) != 1L) {
    stop("`seed` must be a single integer or NULL.", call. = FALSE)
  }
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      if (
        is.null(old) &&
          exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      ) {
        rm(".Random.seed", envir = .GlobalEnv)
      } else if (!is.null(old)) {
        assign(".Random.seed", old, envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  set.seed(as.integer(seed))
  eval.parent(substitute(expr))
}

# Internal helper — not exported. Registers an S3 method for a generic that
# lives in a Suggests package. Uses setHook so the method is registered whether
# the package loads before or after inlaops.
.s3_register <- function(generic, class, method) {
  pieces <- strsplit(generic, "::")[[1L]]
  pkg <- pieces[[1L]]
  genname <- pieces[[2L]]

  do_register <- function(...) {
    registerS3method(genname, class, method, envir = asNamespace(pkg))
  }

  setHook(packageEvent(pkg, "onLoad"), do_register)

  if (isNamespaceLoaded(pkg)) {
    do_register()
  }

  invisible()
}

.onLoad <- function(libname, pkgname) {
  .s3_register(
    "vetiver::vetiver_create_description",
    "bru",
    .vetiver_create_description_bru
  )
  .s3_register("vetiver::vetiver_create_meta", "bru", .vetiver_create_meta_bru)
  .s3_register("vetiver::vetiver_ptype", "bru", .vetiver_ptype_bru)
  .s3_register(
    "vetiver::vetiver_prepare_model",
    "bru",
    .vetiver_prepare_model_bru
  )
  .s3_register("vetiver::handler_startup", "bru", .handler_startup_bru)
  .s3_register("vetiver::handler_predict", "bru", .handler_predict_bru)
  # parsnip-wrapped bru fits are not auto-unwrapped by the bridge — users
  # extract the inner bru fit before calling vetiver_model. See the note
  # in R/vetiver.R for why.

  if (requireNamespace("parsnip", quietly = TRUE)) {
    make_bru_reg()
  }
}
