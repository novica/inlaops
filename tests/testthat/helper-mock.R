# Build a `bru_info` with the inlabru >= 2.15 nesting
# (bru_info$model$lhoods) and classes, so inlabru::as_bru_obs_list()
# dispatches on mocks the same way it does on real fits.
mock_bru_info <- function(lhoods) {
  lhoods <- lapply(lhoods, structure, class = "bru_obs")
  structure(
    list(model = structure(list(lhoods = lhoods), class = "bru_model")),
    class = "bru_info"
  )
}
