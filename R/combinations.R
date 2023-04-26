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

  dots <- match.call(any_satisfied, call, expand.dots = FALSE)$`...`

  errors <- list()

  err1 <- tryCatch({
    eval(substitute(assertthat::assert_that(x),
                    list(x = dots[[1]])))
    NULL
  },
  error = function(e) {
    return(e)
  })

  errors[[1]] <- as.character(err1)

  err2 <- tryCatch({
    eval(substitute(assertthat::assert_that(x),
                    list(x = dots[[2]])))
    NULL
  },
  error = function(e) {
    return(e)
  })

  errors[[2]] <- as.character(err2)

  message <- gsub("^.*?Error: |\n $", "", paste(errors, "", collapse = ""))

  return(message)

}
