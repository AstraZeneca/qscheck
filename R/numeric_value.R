#' Check if the passed entity is a single value which is
#' less than another specified value.
#'
#' @param value the value to check
#' @param comparator the maximum allowed value, exclusive.
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_lt_value(value, comparator))
#' # For check
#' if (qscheck::is_lt_value(value, comparator)) {}
#' }
#'
#' @concept real
#' @export
is_lt_value <- function(
  value, comparator,
  allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_comparison(
    value, comparator, base::`<`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_lt_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  comparator <- callget(call, env, "comparator", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_comparison(
    value, comparator, base::`<`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(paste0(
    call$value,
    snippet_must_be(paste0("numerical value")),
    snippet_comparison(base::`<`, call$comparator),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))

}

#' Check if the passed entity is a single floating point which is
#' less than or equal to another specified value.
#'
#' @param value the value to check
#' @param comparator the maximum allowed value, inclusive.
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_lte_value(value, comparator))
#' # For check
#' if (qscheck::is_lte_value(value, comparator)) {}
#' }
#'
#' @concept real
#' @export
is_lte_value <- function(
  value, comparator,
  allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_comparison(
    value, comparator, base::`<=`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_lte_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  comparator <- callget(call, env, "comparator", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_comparison(
    value, comparator, base::`<=`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(paste0(
    call$value,
    snippet_must_be(paste0("numerical value")),
    snippet_comparison(base::`<=`, call$comparator),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))

}

#' Check if the passed entity is a single numerical value which is
#' greater than another specified value.
#'
#' @param value the value to check
#' @param comparator the minimum allowed value, exclusive.
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_gt_value(value, comparator))
#' # For check
#' if (qscheck::is_gt_value(value, comparator)) {}
#' }
#'
#' @concept real
#' @export
is_gt_value <- function(
    value, comparator,
    allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_comparison(
    value, comparator, base::`>`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_gt_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  comparator <- callget(call, env, "comparator", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_comparison(
    value, comparator, base::`>`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(paste0(
    call$value,
    snippet_must_be(paste0("numerical value")),
    snippet_comparison(base::`>`, call$comparator),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))

}
#' Check if the passed entity is a single numerical value which is
#' greater than or equal to another specified value.
#'
#' @param value the value to check
#' @param comparator the minimum allowed value, inclusive.
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_gte_value(value, comparator))
#' # For check
#' if (qscheck::is_gte_value(value, comparator)) {}
#' }
#'
#' @concept real
#' @export
is_gte_value <- function(
  value, comparator,
  allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_comparison(
    value, comparator, base::`>=`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_gte_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  comparator <- callget(call, env, "comparator", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_comparison(
    value, comparator, base::`>=`,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(paste0(
    call$value,
    snippet_must_be(paste0("numerical value")),
    snippet_comparison(base::`>=`, call$comparator),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}

inspect_comparison <- function(
    value, comparator, operator,
    allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_real_value(comparator)
  check_param("comparator", res$valid, res$reason)

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  res <- inspect_real_value(
    value,
    allow_na = allow_na,
    allow_null = allow_null
  )

  if (!res$valid) {
    return(res)
  }

  if (is_na_value(value)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NA"))
    }
  }


  if (!operator(value, comparator)) {
    return(failure(
      paste0(
        "Passed value ", value, " is",
        snippet_comparison(.reciprocal_op(operator), comparator)
      )
    ))
  }

  return(success())
}

.reciprocal_op <- function(operator) {
  if (identical(operator, base::`<`)) {
    return(base::`>=`)
  } else if (identical(operator, base::`<=`)) {
    return(base::`>`)
  } else if (identical(operator, base::`>`)) {
    return(base::`<=`)
  } else if (identical(operator, base::`>=`)) {
    return(base::`<`)
  }

  stop("Invalid operator in .reciprocal_op")
}
