#' Shorter version of assertthat::assert_that
#'
#' @param \dots Passthrough to assertthat::assert_that
#'
#' @export
assert <- function(...) {
  assertthat::assert_that(...)
}
