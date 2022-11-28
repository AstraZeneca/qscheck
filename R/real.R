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

  if (is.null(value)) {
    return(ifelse(allow_null, TRUE, FALSE))
  }

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  if (is_na_value(value)) {
    return(ifelse(allow_na, TRUE, FALSE))
  }

  if (!is.null(min)) {
    if (inclusive_min && value < min) {
      return(FALSE)
    } else if (!inclusive_min && value <= min) {
      return(FALSE)
    }
  }

  if (!is.null(max)) {
    if (inclusive_max && value > max) {
      return(FALSE)
    } else if (!inclusive_max && value >= max) {
      return(FALSE)
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_real_value) <- function(call, env) {
  min <- callget(call, env, "min", NULL)
  max <- callget(call, env, "max", NULL)
  inclusive_min <- callget(call, env, "inclusive_min", TRUE)
  inclusive_max <- callget(call, env, "inclusive_max", TRUE)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  interval_msg <- ""
  if (!is.null(min) || !is.null(max)) {
    interval_msg <- " in the range "
    if (is.null(min)) {
      interval_msg <- paste0(interval_msg, "(-inf, ")
    } else {
      interval_msg <- paste0(
        interval_msg,
        ifelse(inclusive_min, "[", "("),
        deparse(min), ", ")
    }

    if (is.null(max)) {
      interval_msg <- paste0(interval_msg, "inf)")
    } else {
      interval_msg <- paste0(
        interval_msg, deparse(max),
        ifelse(inclusive_max, "]", ")"))
    }
  }

  return(paste0(deparse(call$value),
                " must be a single real value",
                interval_msg,
                snippet_na(allow_na),
                snippet_null(allow_null),
                ". Got: ",
                deparse(eval(call$value, env))))
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
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  return(paste0(deparse(call$value),
                 " must be a positive real value",
                 snippet_na(allow_na),
                 snippet_null(allow_null),
                 ". Got: ",
                 deparse(eval(call$value, env))))
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
  return(is_real_value(value, min = 0.0, max = 1.0, allow_null = allow_null))
}
assertthat::on_failure(is_probability_value) <- function(call, env) {
  allow_null <- callget(call, env, "allow_null", FALSE)

  return(paste0(deparse(call$value),
                 " must be a single probability value in ",
                 "the interval [0.0, 1.0]",
                 snippet_null(allow_null), ". Got: ",
                 deparse(eval(call$value, env))))
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
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  return(paste0(deparse(call$value),
                 " must be a non-negative real value",
                 snippet_na(allow_na),
                 snippet_null(allow_null),
                 ". Got: ",
                 deparse(eval(call$value, env))))
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

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_real_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a vector of real numbers")
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  if (!is.null(exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", exact_length
    )
  } else if (!is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      min_length,
      " and ",
      max_length,
      " inclusive"
    )
  } else if (is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      max_length
    )
  } else if (!is.null(min_length) && is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      min_length
    )
  }
  if (!allow_na_values) {
    msg <- paste0(msg, " with no NAs")
  }

  msg <- paste0(
    msg,
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
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

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  value <- value[!is.na(value)]

  if (!(all(value > 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_positive_real_vector) <- function(call, env) {
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)
  msg <- paste0(
    deparse(call$value),
    " must be a vector of positive real numbers"
  )

  if (!is.null(exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", exact_length
    )
  } else if (!is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      min_length,
      " and ",
      max_length,
      " inclusive"
    )
  } else if (is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      max_length
    )
  } else if (!is.null(min_length) && is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      min_length
    )
  }
  if (!allow_na_values) {
    msg <- paste0(msg, " with no NAs")
  }

  msg <- paste0(
    msg,
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
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

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  value <- value[!is.na(value)]

  if (!(all(value >= 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_non_negative_real_vector) <- function(call, env) {
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)
  msg <- paste0(
    deparse(call$value),
    " must be a vector of non-negative real numbers"
  )

  if (!is.null(exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", exact_length
    )
  } else if (!is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      min_length,
      " and ",
      max_length,
      " inclusive"
    )
  } else if (is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      max_length
    )
  } else if (!is.null(min_length) && is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      min_length
    )
  }
  if (!allow_na_values) {
    msg <- paste0(msg, " with no NAs")
  }

  msg <- paste0(
    msg,
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
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

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  value <- value[!is.na(value)]

  if (!(all(value >= 0) && all(value <= 1))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_probability_vector) <- function(call, env) {
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)
  msg <- paste0(
    deparse(call$value),
    " must be a vector of values in the interval [0.0, 1.0]"
  )

  if (!is.null(exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", exact_length
    )
  } else if (!is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      min_length,
      " and ",
      max_length,
      " inclusive"
    )
  } else if (is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      max_length
    )
  } else if (!is.null(min_length) && is.null(max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      min_length
    )
  }
  if (!allow_na_values) {
    msg <- paste0(msg, " with no NAs")
  }

  msg <- paste0(
    msg,
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
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
    v, strictly = FALSE, allow_na_values = FALSE) {

  if (!is_real_vector(v, allow_na_values = allow_na_values)) {
    return(FALSE)
  }

  if (any(is.na(v)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  return(!is.unsorted(v, na.rm = TRUE, strictly = strictly))

}
assertthat::on_failure(is_increasing_vector) <- function(call, env) {
  strictly <- callget(call, env, "strictly", FALSE)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  strictly_msg <- ""
  if (strictly) {
    strictly_msg <- " strictly"
  }

  allow_na_values_msg <- " with no NAs"
  if (allow_na_values) {
    allow_na_values_msg <- " or NAs"
  }

  msg <- paste0(
    deparse(call$v), " must be a vector of", strictly_msg,
    " increasing numbers", allow_na_values_msg,
    ". Got: ",
    deparse(eval(call$v, env))
    )

  return(msg)
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
    v, strictly = FALSE, allow_na_values = FALSE) {

  if (!is_real_vector(v, allow_na_values = allow_na_values)) {
    return(FALSE)
  }

  if (any(is.na(v)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  return(!is.unsorted(rev(v), na.rm = TRUE, strictly = strictly))

}
assertthat::on_failure(is_decreasing_vector) <- function(call, env) {
  strictly <- callget(call, env, "strictly", FALSE)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  strictly_msg <- ""
  if (strictly) {
    strictly_msg <- " strictly"
  }

  allow_na_values_msg <- " with no NAs"
  if (allow_na_values) {
    allow_na_values_msg <- " or NAs"
  }

  msg <- paste0(
    deparse(call$v), " must be a vector of", strictly_msg,
    " decreasing numbers", allow_na_values_msg,
    ". Got: ",
    deparse(eval(call$v, env))
    )

  return(msg)
}
