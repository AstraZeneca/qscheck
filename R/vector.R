#' Checks if the passed entity is a vector. Note that in R single values
#' are also vectors.
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
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_vector(my_parameter))
#' # For check
#' if (qscheck::is_vector(my_parameter)) {}
#' }
#'
#' @export
is_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL) {
  if (!is.vector(value)) {
    return(FALSE)
  }
  if (!is.null(exact_length)) {
    return(assertthat::are_equal(length(value), exact_length))
  }

  if (!is.null(min_length)) {
    if (length(value) < min_length) {
      return(FALSE)
    }
  }

  if (!is.null(max_length)) {
    if (length(value) > max_length) {
      return(FALSE)
    }
  }
  return(TRUE)
}
assertthat::on_failure(is_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a vector")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env),
      ". Got: ",
      deparse(eval(call$value, env))

    )
    return(msg)
  }

  if (!is.null(call$min_length) && !is.null(call$max_length)) {
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
  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if the passed entity is a vector of strings (character).
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
#' assertthat::assert_that(qscheck::is_string_vector(my_parameter))
#' # For check
#' if (qscheck::is_string_vector(my_parameter)) {}
#' }
#'
#' @export
is_string_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE) {
  if (!is_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)) {
    return(FALSE)
  }
  if (!is.character(value)) {
    return(FALSE)
  }
  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }
  return(TRUE)
}
assertthat::on_failure(is_string_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a string vector")

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
  if (is.null(call$allow_na_values) ||
      eval(call$allow_na_values, env) == FALSE) {
    msg <- paste0(msg, " with no NAs")
  }

  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if two vectors are of the exact same length.
#'
#' @param v1 the first vector
#' @param v2 the first vector
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::vectors_same_length(v1, v2))
#' # For check
#' if (qscheck::vectors_same_length(v1, v2)) {}
#' }
#'
#' @export
vectors_same_length <- function(v1, v2) {
  if (! (is_vector(v1) && is_vector(v2))) {
      return(FALSE)
  }
  return(assertthat::are_equal(length(v1), length(v2)))
}
assertthat::on_failure(vectors_same_length) <- function(call, env) {
  return(paste0(
    "vectors ", deparse(call$v1), " and ", deparse(call$v2),
    " must have the exact same length."))
}

#' Checks if two vectors are disjoint.
#'
#' @description
#' Allows to check if two vectors are disjoint. Two vectors
#' are disjoint if they are of the same type and none of the elements
#' present in one can be found in the other.
#'
#' @param v1 the first vector
#' @param v2 the second vector
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::vectors_disjoint(v1, v2)
#'   )
#' )
#' # For check
#' if (
#'   qscheck::vectors_disjoint(v1, v2)
#'   ) {
#' }
#' }
#'
#' @export
vectors_disjoint <- function(v1, v2) {
  if (!(is_vector(v1) && is_vector(v2))) {
    return(FALSE)
  }

  if (typeof(v1) != typeof(v2)) {
    return(TRUE)
  }

  return(length(intersect(v1, v2)) == 0)
}
assertthat::on_failure(vectors_disjoint) <- function(call, env) {
  v1 <- eval(call$v1, env)
  v2 <- eval(call$v2, env)
  if (!is_vector(v1)) {
    return(paste0(deparse(call$v1), " must be a vector. Got: ", class(v1)))
  }

  if (!is_vector(v2)) {
    return(paste0(deparse(call$v2), " must be a vector. Got: ", class(v2)))
  }

  common <- intersect(v1, v2)
  return(paste0(
    deparse(call$v1), " and ", deparse(call$v2),
    " must be disjoint vectors but the following elements",
    " are present in both: ", paste0(common, collapse = ", "))
  )
}


#' Checks if a vector contains values only out of a set of available options.
#'
#' @param v the vector
#' @param allowed_values a vector of allowed values
#'
#' @examples
#' \dontrun{
#' # For assertion.
#' # Will fail if v contains any value that is not either 1, 2 or 3
#' assertthat::assert_that(qscheck::vector_allowed_values(v, c(1, 2, 3)))
#' # For check
#' if (qscheck::vector_allowed_values(v, c(1, 2, 3))) {
#' }
#' }
#'
#' @export
vector_allowed_values <- function(v, allowed_values) {
  if (!is_vector(v) || !is_vector(allowed_values)) {
    return(FALSE)
  }

  if (typeof(v) != typeof(allowed_values)) {
    return(FALSE)
  }

  return(all(v %in% allowed_values))
}
assertthat::on_failure(vector_allowed_values) <- function(call, env) {
  return(paste0(
    deparse(call$v),
    " must be a vector containing only elements from the following list: ",
    paste0(
      eval(call$allowed_values, env),
      collapse = ", "
    )
  ))
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
  na_msg <- ""
  degenerate_msg <- ""
  if (!is.null(call$allow_na_values)
      && eval(call$allow_na_values, env) == TRUE) {
    na_msg <- " or NA"
  }

  if (!is.null(call$allow_degenerate)
      && eval(call$allow_degenerate, env) == FALSE) {
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

#' Checks if the passed vector contains no NAs.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_vector_without_na(my_parameter)
#' )
#' # For check
#' if (qscheck::is_vector_without_na(my_parameter)) {}
#' }
#'
#' @export
is_vector_without_na <- function(value) {
  if (!is_vector(value)) {
    return(FALSE)
  }

  if (any(is.na(value))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_vector_without_na) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a vector with no NA values.")

  return(msg)
}

#' Checks if the passed vector contains only NAs.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_vector_all_na(my_parameter)
#' )
#' # For check
#' if (qscheck::is_vector_all_na(my_parameter)) {}
#' }
#'
#' @export
is_vector_all_na <- function(value) {
  if (!is_vector(value)) {
    return(FALSE)
  }

  if (!all(is.na(value))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_vector_all_na) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be a vector containing only NA values."
  )

  return(msg)
}


#' Checks if a given value occurs a given number of times in a vector
#'
#' @param vec The vector to check
#' @param value The value to check
#' @param exact_occurrences the exact number of times the value must be
#'        present. Takes precedence over min/max_occurrences.
#' @param min_occurrences the minimum number of times (inclusive) the
#'                        value must be present.
#' @param max_occurrences the maximum number of times (inclusive) the
#'                        value must be present.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::vector_value_occurrences(
#'     vec, value, min_occurrences = 2, max_occurrences = 3
#'   )
#' )
#' # For check
#' if (qscheck::vector_value_occurrences(
#'     vec, value, min_occurrences = 2, max_occurrences = 3
#'    )) {
#' }
#' }
#'
#' @export
vector_value_occurrences <- function(
      vec,
      value,
      exact_occurrences = NULL,
      min_occurrences = NULL,
      max_occurrences = NULL
      ) {

  if (!is_vector(vec)) {
    return(FALSE)
  }

  total_occurrences <- sum(vec == value, na.rm = TRUE)

  if (!is.null(exact_occurrences)) {
    return(total_occurrences == exact_occurrences)
  }

  if (!is.null(min_occurrences)) {
    if (total_occurrences < min_occurrences) {
      return(FALSE)
    }
  }

  if (!is.null(max_occurrences)) {
    if (total_occurrences > max_occurrences) {
      return(FALSE)
    }
  }

  return(TRUE)

}
assertthat::on_failure(vector_value_occurrences) <- function(call, env) {
  msg <- paste0(
    deparse(call$vec),
    " must be a vector containing value '",
    eval(call$value, env),
    "'"
  )

  total_occurrences <- sum(
    eval(call$vec, env) == eval(call$value, env), na.rm = TRUE
  )

  if (!is.null(call$exact_occurrences)) {
    msg <- paste0(
      msg,
      " exactly ", eval(call$exact_occurrences, env),
      " times. Found it ",
      total_occurrences,
      " times."
    )
    return(msg)
  }

  if (!is.null(call$min_occurrences) && !is.null(call$max_occurrences)) {
    msg <- paste0(
      msg,
      " between ",
      eval(call$min_occurrences, env),
      " and ",
      eval(call$max_occurrences, env),
      " times inclusive."
    )
  } else if (is.null(call$min_occurrences) && !is.null(call$max_occurrences)) {
    msg <- paste0(
      msg,
      " no more than ",
      eval(call$max_occurrences, env),
      " times inclusive."
    )
  } else if (!is.null(call$min_occurrences) && is.null(call$max_occurrences)) {
    msg <- paste0(
      msg,
      " no less than ",
      eval(call$min_occurrences, env),
      " times inclusive."
    )
  }
  msg <- paste0(
    msg,
    " Found it ",
    total_occurrences,
    " times."
  )
  return(msg)
}
