#' Check if the passed entity is a single value, regardless of the type.
#'
#' @param value the value to check
#' @param allow_na if true, accept a value that is NA of any type.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_value(value))
#' # For check
#' if (qscheck::is_value(value)) {}
#' }
#'
#' @concept value
#' @export
is_value <- function(value, allow_na = FALSE, allow_null = FALSE) {
  res <- inspect_value(value, allow_na = allow_na, allow_null = allow_null)
  return(res$valid)
}
assertthat::on_failure(is_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_value(value, allow_na = allow_na, allow_null = allow_null)
  msg <- paste0(
    deparse(call$value),
    snippet_must_be("single value"),
    ". ", res$reason
  )
  return(msg)
}
inspect_value <- function(value, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value cannot be NULL"))
    }
  }

  if (!is.atomic(value)) {
    return(failure("Passed value is not a R atomic vector"))
  }

  if (length(value) != 1) {
    return(failure("Passed value must be a single value, not a vector"))
  }

  if (is.na(value)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("Passed value cannot be NA"))
    }
  }

  return(success())

}


#' Checks if a value is a single NA of any type.
#'
#' @description
#' Allows to check if a given value is a single NA value, regardless
#' of its NA type.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' assertthat::assert_that(
#'   qscheck::is_na_value(value)
#' )
#' }
#'
#' @concept value
#' @export
is_na_value <- function(value) {
  res <- inspect_na_value(value)
  return(res$valid)
}
assertthat::on_failure(is_na_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  res <- inspect_na_value(value)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("NA (any type)"),
    ". ", res$reason)
  return(msg)
}
inspect_na_value <- function(value) {
  res <- inspect_value(
    value, allow_na = TRUE, allow_null = FALSE
  )
  if (!res$valid) {
    return(res)
  }

  if (!is.na(value)) {
    return(failure("Passed value is not NA"))
  }

  return(success())

}



#' Check if the passed entity is a single floating point which is (strictly)
#' greater than another specified value.
#'
#' @param value the value to check
#' @param comparator the minimum allowed value, inclusive or exclusive.
#' @param strictly If TRUE (default), the comparator value is checked exclusive
#' \code{(>)}. If FALSE the comparator value is checked inclusive \code{(>=)}.
#'
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_greater_than_value(value, comparator))
#' # For check
#' if (qscheck::is_greater_than_value(value, comparator)) {}
#' }
#'
#' @concept value
#' @export
is_greater_than_value <- function(value,
    comparator, strictly = TRUE,
    allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_greater_than_value(
    value, comparator,
    strictly = strictly,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_greater_than_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  comparator <- callget(call, env, "comparator", NULL)
  strictly <- callget(call, env, "strictly", TRUE)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_greater_than_value(
    value, comparator,
    strictly = strictly,
    allow_na = allow_na,
    allow_null = allow_null
  )

if (is.null(value) || is.null(comparator)) {
  return(paste(
    res$reason
  ))
} else {
  return(paste0(
    deparse(call$value),
    snippet_must_be(paste0("greater value than ", deparse(call$comparator))),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}
}
inspect_greater_than_value <- function(
  value, comparator, strictly = TRUE,
  allow_na = FALSE, allow_null = FALSE) {

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (is.null(comparator)) {
    return(failure("Passed comparator value is NULL"))
  }

  if (!is.numeric(value)) {
    return(failure("Passed value is not a numerical"))
  }

  if (!is.numeric(comparator)) {
    return(failure("Passed comparator value is not a numerical"))
  }

  if (length(value) != 1) {
    return(failure(
      "Passed value must be a single numerical value, not a vector"
    ))
  }

  if (length(comparator) != 1) {
    return(failure(
      "Passed comparator value must be a single numerical value, not a vector"
    ))
  }

  if (is_na_value(value)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("Passed value was NA"))
    }
  }

  if (is_na_value(comparator)) {
    return(failure("Passed comparator value was NA"))
  }

  if (!is.null(comparator)) {
    if (strictly) {
      if (value <= comparator) {
        return(failure(
          paste0(
            "Passed value ", value, " is below or equal to the minimum of ", comparator)
          )
        )
      }
    } else {
      if (value < comparator) {
        return(failure(
          paste0(
            "Passed value ", value,
            " is below the minimum of ", comparator)
          )
        )
      }
    }
  }

  return(success())
}

#' Check if the passed entity is a single floating point which is (strictly)
#' less than another specified value.
#'
#' @param value the value to check
#' @param comparator the maximum allowed value, inclusive or exclusive.
#' @param strictly If TRUE (default), the comparator value is checked exclusive
#' \code{(<)}. If FALSE the comparator value is checked inclusive \code{(<=)}.
#'
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_less_than_value(value, comparator))
#' # For check
#' if (qscheck::is_less_than_value(value, comparator)) {}
#' }
#'
#' @concept value
#' @export
is_less_than_value <- function(value,
    comparator, strictly = TRUE,
    allow_na = FALSE, allow_null = FALSE) {

  res <- inspect_greater_than_value(
    value, comparator,
    strictly = strictly,
    allow_na = allow_na,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_less_than_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  comparator <- callget(call, env, "comparator", NULL)
  strictly <- callget(call, env, "strictly", TRUE)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_greater_than_value(
    value, comparator,
    strictly = strictly,
    allow_na = allow_na,
    allow_null = allow_null
  )

if (is.null(value) || is.null(comparator)) {
  return(paste(
    res$reason
  ))
} else {
  return(paste0(
    deparse(call$value),
    snippet_must_be(paste0("smaller value than ", deparse(call$comparator))),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}
}
inspect_greater_than_value <- function(
  value, comparator, strictly = TRUE,
  allow_na = FALSE, allow_null = FALSE) {

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (is.null(comparator)) {
    return(failure("Passed comparator value is NULL"))
  }

  if (!is.numeric(value)) {
    return(failure("Passed value is not a numerical"))
  }

  if (!is.numeric(comparator)) {
    return(failure("Passed comparator value is not a numerical"))
  }

  if (length(value) != 1) {
    return(failure(
      "Passed value must be a single numerical value, not a vector"
    ))
  }

  if (length(comparator) != 1) {
    return(failure(
      "Passed comparator value must be a single numerical value, not a vector"
    ))
  }

  if (is_na_value(value)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("Passed value was NA"))
    }
  }

  if (is_na_value(comparator)) {
    return(failure("Passed comparator value was NA"))
  }

  if (!is.null(comparator)) {
    if (strictly) {
      if (value >= comparator) {
        return(failure(
          paste0(
            "Passed value ", value, " is above or equal to the maximum of ", comparator)
          )
        )
      }
    } else {
      if (value > comparator) {
        return(failure(
          paste0(
            "Passed value ", value,
            " is below the maximum of ", comparator)
          )
        )
      }
    }
  }

  return(success())
}