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
dots_pairlist <- match.call(any_satisfied, call, expand.dots = FALSE)$`...`

print(dots_pairlist)
print("type of dots_pairlist")
print(typeof(dots_pairlist))

print(dots_pairlist[[1]])
print("type of dots_pairlist[[1]]")
print(typeof(dots_pairlist[[1]]))

  msg <- paste0(
    paste(deparse(call), collapse = ""),
    ": none of the possible options could be satisfied.")
  return(msg)
}
