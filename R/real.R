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
  allow_na_msg <- ""
  if (!is.null(call$allow_na)) {
    if (eval(call$allow_na, env)) {
      allow_na_msg <- " or NA"
    }
  }

  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  interval_msg <- ""
  if (!is.null(call$min) || !is.null(call$max)) {
    interval_msg <- " in the range "
    if (is.null(call$min)) {
      interval_msg <- paste0(interval_msg, "(-inf, ")
    } else {
      min <- (eval(call$min, env))
      inclusive_min <- (eval(call$inclusive_min, env))
      interval_msg <- paste0(
        interval_msg,
        ifelse(inclusive_min, "[", "("),
        deparse(min), ", ")
    }

    if (is.null(call$max)) {
      interval_msg <- paste0(interval_msg, "inf)")
    } else {
      max <- (eval(call$max, env))
      inclusive_max <- (eval(call$inclusive_max, env))
      interval_msg <- paste0(
        interval_msg, deparse(max),
        ifelse(inclusive_max, "]", ")"))
    }
  }

  return(paste0(deparse(call$value),
                " must be a single real value",
                interval_msg,
                allow_na_msg,
                allow_null_msg,
                ". Got: ",
                deparse(eval(call$value, env))))
}


#' Check if the passed entity is a single floating point positive value.
#'
#' @param value the value to check
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
is_positive_real_value <- function(value) {
  return(is_real_value(value, min = 0.0, inclusive_min = FALSE))
}
assertthat::on_failure(is_positive_real_value) <- function(call, env) {
  return(paste0(deparse(call$value),
                 " must be a positive real value. Got: ",
                 deparse(eval(call$value, env))))
}

#' Check if the passed entity is a single probability value, that is, a
#' floating point value in the interval [0.0, 1.0].
#'
#' @param value the value to check
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
is_probability_value <- function(value) {
  return(is_real_value(value, min = 0.0, max = 1.0))
}
assertthat::on_failure(is_probability_value) <- function(call, env) {
  return(paste0(deparse(call$value),
                 " must be a single probability value in ",
                 "the interval [0.0, 1.0]. Got: ",
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
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
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
    allow_na_values = FALSE) {
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
  if (is.null(call$allow_na_values)
      || eval(call$allow_na_values, env) == FALSE) {
    msg <- paste0(msg, " with no NAs")
  }

  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}
