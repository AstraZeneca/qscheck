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

  if (is.null(value)) {
    return(allow_null)
  }

  if (!assertthat::is.string(value)) {
    return(FALSE)
  }

  if (is_na_value(value)) {
    return(allow_na)
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
  allow_empty <- callget(call, env, "allow_empty", TRUE)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("string"),
    snippet_not_empty(allow_empty),
    snippet_na(allow_na),
    snippet_null(allow_null),
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
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("string vector"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}
