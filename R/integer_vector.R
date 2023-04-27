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
#' @param min the minimum allowed value, inclusive or exclusive.
#' @param inclusive_min if TRUE (default) the min value is checked inclusive.
#' If FALSE, the min value will be checked exclusive.
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
    min = NULL, inclusive_min = TRUE,
    allow_na_values = FALSE, allow_null = FALSE
    ) {
  res <- inspect_integer_vector(
    value, exact_length = exact_length, min_length = min_length,
    max_length = max_length, min = min, inclusive_min = inclusive_min,
    allow_na_values = allow_na_values,
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
    max_length = max_length, min = min, inclusive_min = inclusive_min,
    allow_na_values = allow_na_values,
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
    min = NULL, inclusive_min = TRUE,
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

  if (!is.null(min)) {
    if (inclusive_min) {
      if (any(value) < min) {
        return(failure(
          paste0(
            "Passed vector contains at least one value below ",
            "the minimum of ", min)
          )
        )
      }
    } else {
      if (any(value) <= min) {
        return(failure(
          paste0(
            "Passed vector contains at least one value ",
            "below or equal to the minimum of ", min)
          )
        )
      }
    }
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
#' @param allow_uniform if TRUE (the default), the vector is allowed to contain
#         only one value class (e.g. all the non-NA values are 0, and there's
#         not a single 1, or vice-versa). If FALSE, such vectors will be
#         rejected.
#' @param allow_degenerate deprecated. Use allow_uniform.
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
    v, allow_na_values = FALSE, allow_degenerate = NULL,
    allow_uniform = TRUE
    ) {

  if (!is.null(allow_degenerate)) {
    lifecycle::deprecate_warn(
      "0.23.0",
      "is_binary_vector(allow_degenerate)",
      "is_binary_vector(allow_uniform)"
    )
    allow_uniform <- allow_degenerate
  }

  res <- inspect_binary_vector(
    v, allow_na_values = allow_na_values, allow_uniform = allow_uniform
  )

  return(res$valid)
}
assertthat::on_failure(is_binary_vector) <- function(call, env) {
  v <- callget(call, env, "v", NULL)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)
  allow_degenerate <- callget(call, env, "allow_degenerate", NULL)
  allow_uniform <- callget(call, env, "allow_uniform", TRUE)

  if (!is.null(allow_degenerate)) {
    lifecycle::deprecate_warn(
      "0.23.0",
      "is_binary_vector(allow_degenerate)",
      "is_binary_vector(allow_uniform)"
    )
    allow_uniform <- allow_degenerate
  }

  res <- inspect_binary_vector(v,
    allow_na_values = allow_na_values,
    allow_uniform = allow_uniform)

  return(
    paste0(
      deparse(call$v),
      snippet_must_be("vector of binary values (0 or 1)"),
      snippet_uniform(allow_uniform),
      snippet_na_values(allow_na_values),
      ". ", res$reason
    )
  )
}
inspect_binary_vector <- function(
    v, allow_na_values = FALSE, allow_uniform = TRUE) {
  if (allow_na_values == TRUE) {
    allowed <- c(0, 1, NA)
  } else {
    allowed <- c(0, 1)
  }

  res <- inspect_vector_allowed_values(v, allowed)
  if (!res$valid) {
    return(res)
  }

  if (!allow_uniform) {
    v_entries <- unique(v)
    if (length(v_entries[!is.na(v_entries)]) == 1) {
      return(failure(
        paste0(
          "Passed vector is uniform on the value ", v_entries[[1]]))
      )
    }
  }
  return(success())

}
