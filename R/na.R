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
#' @concept na
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
