#' Checks if the passed value is a single integer value (not type).
#'
#' @param value the value to verify
#' @param min minimum value to constraint the integer, inclusive
#' @param max maximum value to constraint the integer, inclusive
#' @param allow_null if TRUE, accepts a null value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_integer_value(my_parameter))
#' # For check
#' if (qscheck::is_integer_value(my_parameter)) {}
#' }
#'
#' @concept integer
#' @export
is_integer_value <- function(value,
    min = NULL, max = NULL, allow_null = FALSE) {

  res <- inspect_integer_value(
    value, min = min, max = max, allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_integer_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  min <- callget(call, env, "min", NULL)
  max <- callget(call, env, "max", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_integer_value(
    value, min = min, max = max, allow_null = allow_null
  )

  return(paste0(deparse(call$value),
                snippet_must_be("integer value"),
                snippet_numerical_range(min, max),
                snippet_null(allow_null),
                ". ", res$reason
                ))
}

inspect_integer_value <- function(value,
    min = NULL, max = NULL, allow_null = FALSE) {
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

  if ((value %% 1) != 0) {
    return(failure("Passed value must be a whole number (integer)"))
  }

  if (!is.null(min) && value < min) {
    return(failure(
      paste0(
        "Passed value ", value,
        " must be greater than the minimum value ", min
      )
    ))
  }

  if (!is.null(max) && value > max) {
    return(failure(
      paste0("Passed value ", value,
        " must be less than the maximum value ", max
      )))
  }

  return(success())
}

#' Checks if the value is a single positive integer value (not type)
#'
#' @param value the value to verify
#' @param allow_null accepts a null value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_positive_integer_value(my_parameter))
#' # For check
#' if (qscheck::is_positive_integer_value(my_parameter)) {}
#' }
#'
#' @concept integer
#' @export
is_positive_integer_value <- function(value, allow_null = FALSE) {
  res <- inspect_integer_value(value, min = 1, allow_null = allow_null)
  return(res$valid)
}
assertthat::on_failure(is_positive_integer_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_integer_value(value, min = 1, allow_null = allow_null)

  return(
    paste0(
      deparse(call$value),
      snippet_must_be("positive integer value"),
      snippet_null(allow_null),
      ". ", res$reason
    )
  )
}

#' Checks if the value is a single non negative integer value (not type)
#'
#' @param value the value to verify
#' @param allow_null accepts a null value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_non_negative_integer_value(my_parameter))
#' # For check
#' if (qscheck::is_non_negative_integer_value(my_parameter)) {}
#' }
#'
#' @concept integer
#' @export
is_non_negative_integer_value <- function(value, allow_null = FALSE) {
  res <- inspect_integer_value(value, min = 0, allow_null = allow_null)
  return(res$valid)
}
assertthat::on_failure(is_non_negative_integer_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_integer_value(value, min = 0, allow_null = allow_null)
  return(
    paste0(
      deparse(call$value),
      snippet_must_be("non negative integer value"),
      snippet_null(allow_null),
      ". ", res$reason
    )
  )
}
