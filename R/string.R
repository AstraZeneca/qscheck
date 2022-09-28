#' Checks if the passed value is a single string.
#'
#' @param value the value to verify
#' @param allow_empty If TRUE (default) accept an empty string. If FALSE, don't
#' @param allow_na If TRUE, accepts a single NA value. If FALSE (default) don't
#' @param allow_null If TRUE, accepts a single NULL value.
#'                   If FALSE (default) don't
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_string_value(my_parameter))
#' # For check
#' if (qscheck::is_string_value(my_parameter)) {}
#' }
#'
#' @concept string
#' @export
is_string_value <- function(value, allow_empty = TRUE,
                      allow_na = FALSE, allow_null = FALSE) {
  if (is_na_value(value) && allow_na) {
    return(TRUE)
  }

  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!assertthat::is.string(value)) {
    return(FALSE)
  }

  if (nchar(value) == 0) {
    if (allow_empty) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_string_value) <- function(call, env) {
  non_empty_msg <- ""

  allow_empty <- callget(call, env, "allow_empty", TRUE)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)
  if (!allow_empty) {
    non_empty_msg <- " non-empty"
  }

  allow_na_msg <- ""
  if (allow_na) {
    allow_na_msg <- " or NA"
  }

  allow_null_msg <- ""
  if (allow_null) {
    allow_null_msg <- " or NULL"
  }

  msg <- paste0(
    deparse(call$value),
    " must be a",
    non_empty_msg,
    " string",
    allow_na_msg,
    allow_null_msg,
    ". Got: ",
    deparse(eval(call$value, env))
    )
  return(msg)
}

#' Checks if the passed entity is a vector of strings (character).
#' Note that in R single values are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                    *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param allow_na_values boolean. If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#' @param allow_null boolean. If TRUE, also accepts a value of NULL.
#'                   Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_string_vector(my_parameter))
#' # For check
#' if (qscheck::is_string_vector(my_parameter)) {}
#' }
#'
#' @concept vector
#' @export
is_string_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  if (is.null(value)) {
    return(allow_null)
  }

  if (!is_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)) {
    return(FALSE)
  }
  if (!is.character(value)) {
    return(FALSE)
  }
  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }
  return(TRUE)
}
assertthat::on_failure(is_string_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a string vector")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env)
    )
  } else if (!is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      eval(call$min_length, env),
      " and ",
      eval(call$max_length, env),
      " inclusive"
    )
  } else if (is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      eval(call$max_length, env)
    )
  } else if (!is.null(call$min_length) && is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      eval(call$min_length, env)
    )
  }
  if (is.null(call$allow_na_values) ||
      eval(call$allow_na_values, env) == FALSE) {
    msg <- paste0(msg, " with no NAs")
  }

  if (!is.null(call$allow_null) && eval(call$allow_null, env) == TRUE) {
    msg <- paste0(msg, " or NULL")
  }

  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}
