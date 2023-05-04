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

  for (argument in seq_along(dots)) {

    err <- tryCatch({
      eval(substitute(assertthat::assert_that(x),
                      list(x = dots[[argument]])))
      NULL
    },
    error = function(e) {
      return(e)
    })

    errors[[argument]] <- as.character(err)
    errors[[argument]] <- paste(
      strsplit(gsub(".*must be (.+) Passed.*", "\\1", errors[[argument]]), ".", fixed = TRUE)
      , collapse = ',')

  }

  message <- paste0(gsub("^.*?Error: |; \n $", "", paste(
    "None of the following options are satisfied:",
    paste0(errors, "; ", collapse = "")))
  )


  return(message)

}
