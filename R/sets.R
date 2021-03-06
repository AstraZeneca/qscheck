#' Checks if the passed entity is one of the ones in the options vector.
#'
#' @param value the value to check
#' @param options a vector of options.
#'        Value must be one of these for the check not to fail
#' @param allow_null if TRUE, allow the value NULL. If FALSE (default) do not
#'                   allow NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_one_of(value, c("foo", "bar")))
#' # For check
#' if (qscheck::is_one_of(value, c("foo", "bar"))) {}
#' }
#'
#' @concept set
#' @export
is_one_of <- function(value, options, allow_null = NULL) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (typeof(value) != typeof(options)) {
    return(FALSE)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  return(value %in% options)
}
assertthat::on_failure(is_one_of) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }
  msg <- paste0(deparse(call$value), " must be one of the following: '",
                paste0(
                  eval(call$options, env),
                  collapse = "', '"
                  ),
                "'",
                allow_null_msg,
                ". Got: ",
                deparse(eval(call$value, env))
                )
  return(msg)
}

#' Checks if the passed values are mutually exclusive.
#'
#' @description
#' Checks if the passed values are mutually exclusive, that is, if
#' one of the passed values has a non-NULL value, all the others
#' _must_ be NULL.
#' If all options are NULL, this test passes.
#' Allows to check for multiple cases, and pass the test if any of the cases
#' is satisfied.
#' This test is generally useful if two or more parameters in a routine are
#' technically incompatible with each other, and you want the user to know
#' the passed parameters in that combination don't make any sense.
#'
#' @param ... the various values
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::mutually_exclusive(parameter1, parameter2, parameter3)
#' )
#' # For check
#' if (
#'   qscheck::mutually_exclusive(parameter1, parameter2, parameter3)
#' ) {}
#' }
#'
#' @concept set
#' @export
mutually_exclusive <- function(...) {
  dots <- list(...)

  # Prevent empty call
  if (length(dots) == 0) {
    return(FALSE)
  }

  not_null <- dots[!sapply(dots, is.null)]
  return(length(not_null) < 2)
}
assertthat::on_failure(mutually_exclusive) <- function(call, env) {
  args <- as.list(call)

  not_nulls <- list()
  for (i in seq_along(args)) {
    if (i == 1) {
      next
    }
    arg <- args[[i]]
    not_nulls[[arg]] <- eval(call[[i]], env)
  }

  msg <- paste0(
    "'",
    paste0(
      names(not_nulls),
      collapse = "', '"
      ),
    "' must be mutually exclusive. Got ",
    paste0(not_nulls, collapse = ", ")
    )
  return(msg)
}
