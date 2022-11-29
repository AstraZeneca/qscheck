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
is_one_of <- function(value, options, allow_null = FALSE) {
  if (is.null(value)) {
    return(allow_null)
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
  allow_null <- callget(call, env, "allow_null", FALSE)
  options <- callget(call, env, "options", NULL)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be(
      paste0("one of the following: ", flatten_vector(options)),
      article = FALSE
    ),
    snippet_null(allow_null),
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
#' If all options are NULL, by default this test passes. Setting
#' allow_all_null = FALSE changes this behavior to failure. Setting it to
#' FALSE basically ensures that exactly one of the values is not NULL.
#'
#' This test is generally useful if two or more parameters in a routine are
#' technically incompatible with each other, and you want the user to know
#' the passed parameters in that combination don't make any sense.
#'
#' @param ... the various values
#' @param allow_all_null If TRUE, then the test will succeed if all values
#'                       are NULL. If FALSE, it will fail.
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
mutually_exclusive <- function(..., allow_all_null = TRUE) {
  dots <- list(...)

  # Prevent empty call
  if (length(dots) == 0) {
    return(FALSE)
  }
  not_null <- dots[!sapply(dots, is.null)]

  if (allow_all_null) {
    return(length(not_null) < 2)
  } else {
    return(length(not_null) == 1)
  }
}
assertthat::on_failure(mutually_exclusive) <- function(call, env) {
  args <- as.list(call)

  # Remove the keyword argument, leaving only the matched by ...
  args[["allow_all_null"]] <- NULL

  not_null_val <- list()
  not_null_idx <- numeric()
  for (i in seq_along(args)) {
    if (i == 1) {
      next
    }
    value <- eval(call[[i]], env)
    if (!is.null(value)) {
      not_null_val[[i]] <- value
      not_null_idx <- c(not_null_idx, i)
    }
  }

  allow_all_null <- callget(call, env, "allow_all_null", TRUE)

  allow_all_null_msg <- " or all NULL"
  if (!allow_all_null) {
    allow_all_null_msg <- " with exactly one non-NULL element"
  }

  if (length(not_null_idx) == 0 && !allow_all_null) {
    msg <- paste0(
      flatten_vector(tail(args, -1)),
      " must be mutually exclusive", allow_all_null_msg, ". Got all NULLs"
    )
    return(msg)
  }

  msg <- paste0(
    flatten_vector(args[not_null_idx]),
    " must be mutually exclusive", allow_all_null_msg, ". Got ",
    flatten_vector(not_null_val[not_null_idx])
    )
  return(msg)
}
