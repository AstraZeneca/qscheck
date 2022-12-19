#' Checks if the passed value is a single integer value (not type)
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

#' Checks if the passed entity is a vector of integers. NOTE:  NOT of integer
#' type. Numerics that are whole numbers.
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
#' assertthat::assert_that(
#'   qscheck::is_integer_vector(my_parameter)
#' )
#' # For check
#' if (qscheck::is_integer_vector(my_parameter)) {}
#' }
#'
#' @concept integer
#' @export
is_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE
    ) {
  res <- inspect_integer_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values,
    allow_null = allow_null
  )
  return(res$valid)
}
assertthat::on_failure(is_integer_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_integer_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values,
    allow_null = allow_null
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of integer values"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}

inspect_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE
) {

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  res <- inspect_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length
  )

  if (!res$valid) {
    return(res)
  }

  if (!is.numeric(value)) {
    return(failure("Passed vector is not a numerical vector"))
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(failure("Passed vector contains NAs"))
  }

  value <- value[!is.na(value)]

  if (!(all(value %% 1 == 0))) {
    return(failure("Passed vector contains non integer values"))
  }

  return(success())
}

#' Checks if the passed entity is a vector of positive integers.
#' Note that in R single values are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                     *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_positive_integer_vector(my_parameter)
#' )
#' # For check
#' if (qscheck::is_positive_integer_vector(my_parameter)) {}
#' }
#'
#' @concept integer
#' @export
is_positive_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

  res <- inspect_positive_integer_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length,
    allow_na_values = allow_na_values
  )

  return(res$valid)
}
assertthat::on_failure(is_positive_integer_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  res <- inspect_positive_integer_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length,
    allow_na_values = allow_na_values
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of positive integer values"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    ". ", res$reason
  )
  return(msg)
}

inspect_positive_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
) {
  res <- inspect_integer_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values
  )

  if (!res$valid) {
    return(res)
  }

  value <- value[!is.na(value)]

  if (!(all(value %% 1 == 0) && all(value > 0))) {
    return(failure(
      "Passed vector contain values that are not positive integers"
    ))
  }

  return(success())

}

#' Checks if the passed entity is a vector of non negative integers.
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
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_non_negative_integer_vector(my_parameter)
#' )
#' # For check
#' if (qscheck::is_non_negative_integer_vector(my_parameter)) {}
#' }
#'
#' @concept integer
#' @export
is_non_negative_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

  res <- inspect_non_negative_integer_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length,
    allow_na_values = allow_na_values
  )

  return(res$valid)
}
assertthat::on_failure(is_non_negative_integer_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  res <- inspect_non_negative_integer_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length,
    allow_na_values = allow_na_values
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of non negative integer values"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    ". ", res$reason
  )
  return(msg)
}

inspect_non_negative_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
) {
  res <- inspect_integer_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values
  )

  if (!res$valid) {
    return(res)
  }

  value <- value[!is.na(value)]

  if (!(all(value %% 1 == 0) && all(value >= 0))) {
    return(failure(
      "Passed vector contain values that are negative"
    ))
  }

  return(success())

}

#' Checks if a vector contains only binary values (0 or 1)
#'
#' @param v the vector
#' @param allow_na_values if the vector can contain NA values. Default FALSE
#' @param allow_degenerate if TRUE, the vector can contain only one value class
#'        (e.g. all the non-NA values are 0, and there's not a single 1, or
#'        vice-versa). If FALSE, such vectors will be rejected.
#'
#' @examples
#' \dontrun{
#' # For assertion.
#' # Will fail if v contains any value that is not either 0 or 1
#' assertthat::assert_that(qscheck::is_binary_vector(v))
#' # For check
#' if (qscheck::is_binary_vector(v)) {}
#' }
#'
#' @concept vector
#' @export
is_binary_vector <- function(
    v, allow_na_values = FALSE, allow_degenerate = TRUE
    ) {

  res <- inspect_binary_vector(
    v, allow_na_values = allow_na_values, allow_degenerate = allow_degenerate
  )

  return(res$valid)
}
assertthat::on_failure(is_binary_vector) <- function(call, env) {
  v <- callget(call, env, "v", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_degenerate <- callget(call, env, "allow_degenerate", TRUE)

  res <- inspect_binary_vector(v)

  return(
    paste0(
      deparse(call$v),
      snippet_must_be("vector of binary values (0 or 1)"),
      snippet_degenerate(allow_degenerate),
      snippet_na_values(allow_na_values),
      ". ", res$reason
    )
  )
}
inspect_binary_vector <- function(
    v, allow_na_values = FALSE, allow_degenerate = TRUE) {
  if (allow_na_values == TRUE) {
    allowed <- c(0, 1, NA)
  } else {
    allowed <- c(0, 1)
  }

  res <- inspect_vector_allowed_values(v, allowed)
  if (!res$valid) {
    return(res)
  }

  if (!allow_degenerate) {
    v_entries <- unique(v)
    if (length(v_entries[!is.na(v_entries)]) == 1) {
      return(failure(
        paste0(
          "Passed vector is degenerate on the value ", v_entries[[1]]))
      )
    }
  }
  return(success())

}
