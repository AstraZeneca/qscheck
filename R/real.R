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
      return(failure("Passed value was NA"))
    }
  }

  if (!is.null(min)) {
    if (inclusive_min) {
      if (value < min) {
        return(failure(
          paste0(
            "Passed value ", value, " is below the minimum of ", min)
          )
        )
      }
    } else {
      if (value <= min) {
        return(failure(
          paste0(
            "Passed value ", value,
            " is below or equal to the minimum of ", min)
          )
        )
      }
    }
  }

  if (!is.null(max)) {
    if (inclusive_max) {
      if (value > max) {
        return(failure(
          paste0(
            "Passed value ", value, " is above the maximum of ", max)
          )
        )
      }
    } else {
      if (value >= max) {
        return(failure(
          paste0(
            "Passed value ", value,
            " is above or equal to the maximum of ", max)
          )
        )
      }
    }
  }

  return(success())
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


#' Checks if the passed entity is a vector of reals (numeric).
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
#' assertthat::assert_that(qscheck::is_real_vector(my_parameter))
#' # For check
#' if (qscheck::is_real_vector(my_parameter)) {}
#' }
#'
#' @concept real
#' @export
is_real_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  res <- inspect_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_real_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of real numbers"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}
inspect_real_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

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

  res <- inspect_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)

  if (!res$valid) {
    return(res)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(failure("Vector contains NA values, but they are not allowed"))
  }

  return(success())
}


#' Checks if the passed entity is a vector of positive reals (numeric).
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
#' assertthat::assert_that(qscheck::is_positive_real_vector(my_parameter))
#' # For check
#' if (qscheck::is_positive_real_vector(my_parameter)) {}
#' }
#'
#' @concept real
#' @export
is_positive_real_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  res <- inspect_positive_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )
}
assertthat::on_failure(is_positive_real_vector) <- function(call, env) {
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_positive_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of positive real numbers"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}
inspect_positive_real_vector <- function(
  value, exact_length = exact_length,
  min_length = min_length, max_length = max_length,
  allow_na_values = allow_na_values, allow_null = allow_null
) {

  res <- inspect_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )
  if (!res$valid) {
    return(res)
  }
  value <- value[!is.na(value)]

  if (!(all(value > 0))) {
    return(failure("Some values are not positive"))
  }

  return(success())

}
#' Checks if the passed entity is a vector of non-negative reals (numeric).
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
#' assertthat::assert_that(qscheck::is_non_negative_real_vector(my_parameter))
#' # For check
#' if (qscheck::is_non_negative_real_vector(my_parameter)) {}
#' }
#'
#' @concept real
#' @export
is_non_negative_real_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  res <- inspect_non_negative_real_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values,
    allow_null = allow_null
  )

  return(res$valid)
}
assertthat::on_failure(is_non_negative_real_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_non_negative_real_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values,
    allow_null = allow_null
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of non-negative real numbers"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}
inspect_non_negative_real_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  res <- inspect_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )
  if (!res$valid) {
    return(res)
  }

  value <- value[!is.na(value)]

  if (!(all(value >= 0))) {
    return(failure("Some values are negative"))
  }

  return(success())
}
#' Checks if the passed entity is a vector of probabilities, that is, a
#' vector of values in the interval [0.0, 1.0].
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
#' assertthat::assert_that(qscheck::is_probability_vector(my_parameter))
#' # For check
#' if (qscheck::is_probability_vector(my_parameter)) {}
#' }
#'
#' @concept real
#' @export
is_probability_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  res <- inspect_probability_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values,
    allow_null = allow_null
  )
  return(res$valid)
}
assertthat::on_failure(is_probability_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_probability_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, allow_na_values = allow_na_values,
    allow_null = allow_null
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector of values in the interval [0.0, 1.0]"),
    snippet_length(exact_length, min_length, max_length),
    snippet_na_values(allow_na_values),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}
inspect_probability_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE, allow_null = FALSE) {

  res <- inspect_real_vector(
    value, exact_length = exact_length,
    min_length = min_length, max_length = max_length,
    allow_na_values = allow_na_values, allow_null = allow_null
  )
  if (!res$valid) {
    return(res)
  }

  value <- value[!is.na(value)]

  if (!(all(value >= 0) && all(value <= 1))) {
    return(failure("Some values were outside the allowed range"))
  }

  return(success())

}
#' Checks if the passed entity is a vector of increasing numerical values.
#'
#' @param v The vector to check
#' @param strictly boolean: If TRUE, only strictly increasing vectors will
#'                          satisfy the test
#' @param allow_na_values boolean: If TRUE, NA values are allowed in the vector
#'                                 and are considered as not existent.
#'                                 If FALSE, fails for vectors containing NAs.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_increasing_vector(my_parameter))
#' # For check
#' if (qscheck::is_increasing_vector(my_parameter)) {}
#' }
#'
#' @concept vector
#' @export
is_increasing_vector <- function(
    v, strictly = FALSE, allow_na_values = FALSE
    ) {

  res <- inspect_increasing_vector(
    v, strictly = strictly, allow_na_values = allow_na_values
  )

  return(res$valid)

}
assertthat::on_failure(is_increasing_vector) <- function(call, env) {
  v <- callget(call, env, "v", NULL)
  strictly <- callget(call, env, "strictly", FALSE)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  res <- inspect_increasing_vector(
    v, strictly = strictly, allow_na_values = allow_na_values
  )

  strictly_msg <- ""
  if (strictly) {
    strictly_msg <- " strictly"
  }

  msg <- paste0(
    deparse(call$v),
    snippet_must_be(paste0("vector of", strictly_msg, " increasing numbers")),
    snippet_na_values(allow_na_values),
    ". ", res$reason
  )

  return(msg)
}
inspect_increasing_vector <- function(
    v, strictly = FALSE, allow_na_values = FALSE
) {
  res <- inspect_real_vector(v, allow_na_values = allow_na_values)

  if (!res$valid) {
    return(res)
  }

  strictly_msg <- ""
  if (strictly) {
    strictly_msg <- " strictly"
  }

  if (!is.unsorted(v, na.rm = TRUE, strictly = strictly)) {
    return(failure(
      paste0("Passed vector is not", strictly_msg, " increasing")))
  }

  return(success())

}

#' Checks if the passed entity is a vector of decreasing numerical values.
#'
#' @param v The vector to check
#' @param strictly boolean: If TRUE, only strictly decreasing vectors will
#'                          satisfy the test.
#' @param allow_na_values boolean: If TRUE, NA values are allowed in the vector
#'                                 and are considered as not existent.
#'                                 If FALSE, fails for vectors containing NAs.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_decreasing_vector(my_parameter))
#' # For check
#' if (qscheck::is_decreasing_vector(my_parameter)) {}
#' }
#'
#' @concept vector
#' @export
is_decreasing_vector <- function(
    v, strictly = FALSE, allow_na_values = FALSE
) {

  res <- inspect_decreasing_vector(
    v, strictly = strictly, allow_na_values = allow_na_values
  )

  return(res$valid)
}
assertthat::on_failure(is_decreasing_vector) <- function(call, env) {
  v <- callget(call, env, "v", NULL)
  strictly <- callget(call, env, "strictly", FALSE)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  res <- inspect_decreasing_vector(
    v, strictly = strictly, allow_na_values = allow_na_values
  )

  strictly_msg <- ""
  if (strictly) {
    strictly_msg <- " strictly"
  }

  msg <- paste0(
    deparse(call$v),
    snippet_must_be(paste0("vector of", strictly_msg, " decreasing numbers")),
    snippet_na_values(allow_na_values),
    ". ", res$reason
    )

  return(msg)
}
inspect_decreasing_vector <- function(
    v, strictly = FALSE, allow_na_values = FALSE
) {
  res <- inspect_real_vector(v, allow_na_values = allow_na_values)

  if (!res$valid) {
    return(res)
  }

  strictly_msg <- ""
  if (strictly) {
    strictly_msg <- " strictly"
  }

  if (!is.unsorted(rev(v), na.rm = TRUE, strictly = strictly)) {
    return(failure(
      paste0("Passed vector is not", strictly_msg, " decreasing")))
  }

}
