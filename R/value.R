#' Check if the passed entity is a single value, regardless of the type.
#'
#' @param value the value to check
#' @param allow_na if true, accept a value that is NA of any type.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_value(value))
#' # For check
#' if (qscheck::is_value(value)) {}
#' }
#'
#' @concept value
#' @export
is_value <- function(value, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(value)) {
    return(allow_null)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  if (is_na_value(value)) {
    return(ifelse(allow_na, TRUE, FALSE))
  }

  return(TRUE)
}
assertthat::on_failure(is_value) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be a single value",
    ". Got: ",
    deparse(eval(call$value, env)))
  return(msg)
}


#' Checks if a value is a single NA of any type.
#'
#' @description
#' Allows to check if a given value is a single NA value, regardless
#' of its NA type.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' assertthat::assert_that(
#'   qscheck::is_na_value(value)
#' )
#' }
#'
#' @concept value
#' @export
is_na_value <- function(value) {
  return(is.vector(value) && length(value) == 1 && is.na(value))
}
assertthat::on_failure(is_na_value) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be NA",
    ". Got: ",
    deparse(eval(call$value, env)))
  return(msg)
}
