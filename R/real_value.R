#' Check if the passed entity is a single floating point value.
#' Optionally, specify the minimum and maximum values allowed, either
#' inclusive or exclusive.
#'
#' @param value the value to check
#' @param min the minimum allowed value, inclusive or exclusive.
#' @param max the maximum allowed value, inclusive or exclusive.
#' @param inclusive_min if TRUE (default) the min value is checked inclusive.
#'        If FALSE, the min value will be checked exclusive.
#' @param inclusive_max if TRUE (default) the max value is checked inclusive.
#'        If FALSE, the max value will be checked exclusive
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_real_value(value))
#' # For check
#' if (qscheck::is_real_value(value)) {}
#' }
#'
#' @concept real
#' @export
is_real_value <- function(value,
    min = NULL, max = NULL,
    inclusive_min = TRUE, inclusive_max = TRUE,
    allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_real_value(
    value, min = min, max = max,
    inclusive_min = inclusive_min,
    inclusive_max = inclusive_max,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_real_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  min <- callget(call, env, "min", NULL)
  max <- callget(call, env, "max", NULL)
  inclusive_min <- callget(call, env, "inclusive_min", TRUE)
  inclusive_max <- callget(call, env, "inclusive_max", TRUE)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_real_value(
    value, min = min, max = max,
    inclusive_min = inclusive_min,
    inclusive_max = inclusive_max,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(paste0(
    deparse(call$value),
    snippet_must_be("real value"),
    snippet_numerical_range(min, max, inclusive_min, inclusive_max),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}
inspect_real_value <- function(value,
    min = NULL, max = NULL,
    inclusive_min = TRUE, inclusive_max = TRUE,
    allow_na = FALSE, allow_null = FALSE) {

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (!is.numeric(value)) {
    return(failure("Passed value is not a numerical"))
  }

  if (length(value) != 1) {
    return(failure(
      "Passed value must be a single numerical value, not a vector"
    ))
  }

  if (is_na_value(value)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NA"))
    }
  }

  return(
    check_limits(
      value, min = min, max = max,
      inclusive_min = inclusive_min, inclusive_max = inclusive_max
    )
  )

}
#' Check if the passed entity is a single floating point positive value.
#'
#' @param value the value to check
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_positive_real_value(value))
#' # For check
#' if (qscheck::is_positive_real_value(value)) {}
#' }
#'
#' @concept real
#' @export
is_positive_real_value <- function(
    value, allow_na = FALSE, allow_null = FALSE) {
  return(is_real_value(value, min = 0.0, inclusive_min = FALSE,
          allow_na = allow_na, allow_null = allow_null))
}
assertthat::on_failure(is_positive_real_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_real_value(
    value, min = 0.0, inclusive_min = FALSE,
    allow_na = allow_na, allow_null = allow_null)

  return(paste0(deparse(call$value),
                 snippet_must_be("positive real value"),
                 snippet_na(allow_na),
                 snippet_null(allow_null),
                 ". ", res$reason))
}

#' Check if the passed entity is a single probability value, that is, a
#' floating point value in the interval [0.0, 1.0].
#'
#' @param value the value to check
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_probability_value(value))
#' # For check
#' if (qscheck::is_probability_value(value)) {}
#' }
#'
#' @concept real
#' @export
is_probability_value <- function(value, allow_null = FALSE) {
  return(is_real_value(
    value, min = 0.0, max = 1.0,
    inclusive_min = TRUE, inclusive_max = TRUE,
    allow_null = allow_null))
}
assertthat::on_failure(is_probability_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_real_value(
    value, min = 0.0, max = 1.0,
    inclusive_min = TRUE, inclusive_max = TRUE,
    allow_null = allow_null)

  return(paste0(deparse(call$value),
                 snippet_must_be(
                  "probability value in the interval [0.0, 1.0]"
                 ),
                 snippet_null(allow_null),
                 ". ", res$reason
                 ))
}


#' Check if the passed entity is a single floating point non-negative value.
#'
#' @param value the value to check
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_non_negative_real_value(value))
#' # For check
#' if (qscheck::is_non_negative_real_value(value)) {}
#' }
#'
#' @concept real
#' @export
is_non_negative_real_value <- function(
    value, allow_na = FALSE, allow_null = FALSE) {
  return(is_real_value(value, min = 0.0, inclusive_min = TRUE,
          allow_na = allow_na, allow_null = allow_null))
}
assertthat::on_failure(is_non_negative_real_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_real_value(
    value, min = 0.0, inclusive_min = TRUE,
    allow_na = allow_na, allow_null = allow_null)

  return(paste0(deparse(call$value),
                 snippet_must_be("non-negative real value"),
                 snippet_na(allow_na),
                 snippet_null(allow_null),
                 ". ", res$reason
                 ))
}
