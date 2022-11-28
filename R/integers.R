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
  if (is.null(value)) {
    return(allow_null)
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
  allow_null <- callget(call, env, "allow_null", FALSE)
  min <- callget(call, env, "min", NULL)
  max <- callget(call, env, "max", NULL)

  allow_null_msg <- ""
  if (allow_null) {
    allow_null_msg <- " or NULL"
  }

  interval_msg <- ""
  if (!is.null(min) || !is.null(max)) {
    interval_msg <- " in the range "
    if (is.null(min)) {
      interval_msg <- paste0(interval_msg, "(-inf, ")
    } else {
      interval_msg <- paste0(interval_msg, "[", min, ", ")
    }

    if (is.null(max)) {
      interval_msg <- paste0(interval_msg, "inf)")
    } else {
      interval_msg <- paste0(interval_msg, max, "]")
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
#' @concept integer
#' @export
is_positive_integer_value <- function(value, allow_null = FALSE) {
  if (is.null(value)) {
    return(allow_null)
  }
  return(is_integer_value(value) && (value > 0))
}
assertthat::on_failure(is_positive_integer_value) <- function(call, env) {
  allow_null <- callget(call, env, "allow_null", FALSE)
  allow_null_msg <- ""
  if (allow_null) {
    allow_null_msg <- " or NULL"
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
#' @concept integer
#' @export
is_non_negative_integer_value <- function(value, allow_null = FALSE) {
  if (is.null(value)) {
    return(allow_null)
  }
  return(is_integer_value(value) && (value >= 0))
}
assertthat::on_failure(is_non_negative_integer_value) <- function(call, env) {
  allow_null <- callget(call, env, "allow_null", FALSE)
  allow_null_msg <- ""

  if (allow_null) {
    allow_null_msg <- " or NULL"
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

  if (!(all(value %% 1 == 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_integer_vector) <- function(call, env) {
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  msg <- paste0(deparse(call$value),
    " must be a vector of integer values")

  if (!is.null(exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", exact_length)
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
  if (allow_null) {
    msg <- paste0(msg, " or NULL")
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
#' @concept integer
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
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  msg <- paste0(deparse(call$value),
    " must be a vector of positive integer values")

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
#' @concept integer
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
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  msg <- paste0(deparse(call$value),
    " must be a vector of non negative integer values")

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
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
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

  if (allow_na_values == TRUE) {
    allowed <- c(0, 1, NA)
  } else {
    allowed <- c(0, 1)
  }

  if (!vector_allowed_values(v, allowed)) {
    return(FALSE)
  }


  if (!allow_degenerate) {
    v_entries <- unique(v)
    if (length(v_entries[!is.na(v_entries)]) == 1) {
      return(FALSE)
    }
  }
  return(TRUE)
}
assertthat::on_failure(is_binary_vector) <- function(call, env) {
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_degenerate <- callget(call, env, "allow_degenerate", TRUE)

  na_msg <- ""
  degenerate_msg <- ""
  if (allow_na_values) {
    na_msg <- " or NA"
  }

  if (!allow_degenerate) {
    degenerate_msg <- " non-degenerate"
  }

  return(
    paste0(
      deparse(call$v),
      " must be a", degenerate_msg,
      " vector of binary values (0 or 1",
      na_msg, ")"
    )
  )
}
