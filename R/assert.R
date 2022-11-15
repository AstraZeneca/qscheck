#' Shorter version of assertthat::assert_that
#'
#' @param \dots Passthrough to assertthat::assert_that
#'
#' @export
assert <- function(..., env = parent.frame(), msg = NULL) {
  assertthat::assert_that(..., env = env, msg = msg)
}
