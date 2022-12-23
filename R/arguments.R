#' Checks consequential presence of arguments.
#'
#' @description
#' Check that passes in either of the two cases:
#' - if the first parameter (anchor) is not NULL, then
#'   the second parameter (dependent) must also not be NULL, or
#' - if the first parameter (anchor) is NULL, then
#'   the second parameter (dependent) must also be NULL
#'
#' This test is generally useful if one or more parameters in a routine are
#' required if another parameter is passed, and are therefore consequential
#' to its presence.
#'
#' @param anchor The parameter that decides if the other must be
#'               NULL or not.
#' @param dependent The parameter that is dependent on anchor being NULL or not
#'
#' @examples
#' \dontrun{
#' # For assertion. This passes if either is satisfied:
#' # If a is not NULL, then b must also be not NULL.
#' # If a is NULL, then b must also be NULL.
#' assertthat::assert_that(
#'   qscheck::consequential(a, b)
#' )
#' # For check
#' if (
#'   qscheck::consequential(a, b)
#' ) {}
#' }
#'
#' @concept arguments
#' @export
consequential <- function(anchor, dependent) {
  res <- inspect_consequential(anchor, dependent)
  return(res$valid)
}
assertthat::on_failure(consequential) <- function(call, env) {
  anchor <- callget(call, env, "anchor", NULL)
  dependent <- callget(call, env, "dependent", NULL)

  res <- inspect_consequential(anchor, dependent)

  if (is.null(anchor)) {
    not_msg <- ""
  } else {
    not_msg <- " not"
  }
  msg <- paste0(
    "If ",
    deparse(call$anchor),
    " is", not_msg, " NULL, ",
    deparse(call$dependent),
    " must also be", not_msg, " NULL. ",
    res$reason
  )
  return(msg)
}

inspect_consequential <- function(anchor, dependent) {
  if (is.null(anchor) && !is.null(dependent)) {
    return(failure("The second argument is not NULL"))
  } else if (!is.null(anchor) && is.null(dependent)) {
    return(failure("The second argument is NULL"))
  }

  return(success())
}
