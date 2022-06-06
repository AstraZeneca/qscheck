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
#' @concept integers
#' @export
is_integer_value <- function(value,
    min = NULL, max = NULL, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  if ((value %% 1) != 0) {
    return(FALSE)
  }

  if (!is.null(min) && value < min) {
    return(FALSE)
  }

  if (!is.null(max) && value > max) {
    return(FALSE)
  }

  return(TRUE)

}
assertthat::on_failure(is_integer_value) <- function(call, env) {
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
      interval_msg <- paste0(interval_msg, "[", deparse(min), ", ")
    }

    if (is.null(call$max)) {
      interval_msg <- paste0(interval_msg, "inf)")
    } else {
      max <- (eval(call$max, env))
      interval_msg <- paste0(interval_msg, deparse(max), "]")
    }
  }

  return(paste0(deparse(call$value),
                " must be an integer value",
                interval_msg,
                allow_null_msg,
                ". Got: ",
                deparse(eval(call$value, env))))
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
#' @concept integers
#' @export
is_positive_integer_value <- function(value, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }
  return(is_integer_value(value) && (value > 0))
}
assertthat::on_failure(is_positive_integer_value) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }
  return(
    paste0(
      deparse(call$value),
      " must be a positive integer value",
      allow_null_msg,
      ". Got: ",
      deparse(eval(call$value, env))
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
#' @concept integers
#' @export
is_non_negative_integer_value <- function(value, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }
  return(is_integer_value(value) && (value >= 0))
}
assertthat::on_failure(is_non_negative_integer_value) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }
  return(
    paste0(
      deparse(call$value),
      " must be a non negative integer value",
      allow_null_msg,
      ". Got: ",
      deparse(eval(call$value, env))
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
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
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
#' @concept integers
#' @export
is_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

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

  if (!(all(value %% 1 == 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_integer_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value),
    " must be a vector of integer values")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env))
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
#' @concept integers
#' @export
is_positive_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

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

  if (!(all(value %% 1 == 0) && all(value > 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_positive_integer_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value),
    " must be a vector of positive integer values")

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
#' @concept integers
#' @export
is_non_negative_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

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

  if (!(all(value %% 1 == 0) && all(value >= 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_non_negative_integer_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value),
    " must be a vector of non negative integer values")

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
