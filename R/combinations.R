#' Checks if any one of a set of assertions is satisfied
#'
#' @description
#' Allows to check for multiple cases, and pass the test if any of the cases
#' is satisfied.
#'
#' @param ... the various assertions that can be satisfied
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::any_satisfied(
#'    qscheck::is_positive_integer_value(value),
#'    qscheck::is_list(value)
#'   )
#' )
#' # For check
#' if (
#'   qscheck::any_satisfied(
#'    qscheck::is_positive_integer_value(value),
#'    qscheck::is_list(value)
#'   )
#' ) {}
#' }
#'
#' @concept combined
#' @export
any_satisfied <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    return(FALSE)
  }

  return(any(as.logical(dots)))
}
assertthat::on_failure(any_satisfied) <- function(call, env) {
  msg <- paste0(
    paste(deparse(call), collapse = ""),
    ": none of the possible options could be satisfied.")
  return(msg)
}
