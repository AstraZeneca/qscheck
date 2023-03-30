#' Shorter version of assertthat::assert_that
#'
#' @param \dots Passthrough to assertthat::assert_that
#'
#' @export
assert <- function(..., env = parent.frame(), msg = NULL) {
  assertthat::assert_that(..., env = env, msg = msg)
}

#' Returns just the error message or NULL if no error.
#'
#' @param \dots Condition to test
#'
#' @export
message <- function(..., env = parent.frame(), msg = NULL) {
  res <- assertthat::validate_that(..., env = env, msg = msg)
  if (res == TRUE) {
    return(NULL)
  }
  return(res)
}
