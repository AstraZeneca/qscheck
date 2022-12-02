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
#' @concept vector
#' @export
is_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL) {

  res <- inspect_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length
  )
  return(res$valid)
}
assertthat::on_failure(is_vector) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  min_length <- callget(call, env, "min_length", NULL)
  max_length <- callget(call, env, "max_length", NULL)

  res <- inspect_vector(
    value,
    exact_length = exact_length,
    min_length = min_length,
    max_length = max_length
  )

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("vector"),
    snippet_length(exact_length, min_length, max_length),
    ". ", res$reason
  )

  return(msg)
}

inspect_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL) {

  if (!is.vector(value)) {
    return(failure("Passed value is not a vector"))
  }

  if (!is.null(exact_length)) {
    if (length(value) != exact_length) {
      return(failure(
        paste0(
          "Passed vector length is ", length(value),
          " instead of the expected ", exact_length
        )
      ))
    } else {
      return(success())
    }
  }

  if (!is.null(min_length)) {
    if (length(value) < min_length) {
      return(failure(paste0(
        "Passed vector length is ", length(value),
        " but must be at least ", min_length
      )))
    }
  }

  if (!is.null(max_length)) {
    if (length(value) > max_length) {
      return(failure(paste0(
        "Passed vector length is ", length(value),
        " but must be at most ", max_length
      )))
    }
  }
  return(success())
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
#' @concept vector
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
#' # For check
#' if (qscheck::vectors_disjoint(v1, v2)) {
#' }
#' }
#'
#' @concept vector
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
#' @concept vector
#' @export
vector_allowed_values <- function(v, allowed_values) {
  res <- inspect_vector_allowed_values(v, allowed_values)

  return(res$valid)
}
assertthat::on_failure(vector_allowed_values) <- function(call, env) {
  v <- callget(call, env, "v", NULL)
  allowed_values <- callget(call, env, "allowed_values", NULL)

  res <- inspect_vector_allowed_values(v, allowed_values)

  return(paste0(
    deparse(call$v),
    snippet_must_be("vector"),
    " containing only elements from the following list: ",
    flatten_vector(allowed_values), ". ", res$reason
  ))
}

inspect_vector_allowed_values <- function(v, allowed_values) {
  res <- inspect_vector(v)
  if (!res$valid) {
    return(failure(paste0(
      "Argument v has the following issue: ", res$reason
    )))
  }

  res <- inspect_vector(allowed_values)
  if (!res$valid) {
    return(failure(paste0(
      "Argument allowed_values has the following issue: ", res$reason
    )))
  }

  if (typeof(v) != typeof(allowed_values)) {
    return(failure(
      paste0(
        "Arguments have different types: v is ", typeof(v),
        " and allowed_values is ", typeof(allowed_values)
      )
    ))
  }

  if (!all(v %in% allowed_values)) {
    return(failure(
      "Vector contains elements that are not in the allowed_values"))
  }

  return(success())
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
#' @concept vector
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
  msg <- paste0(deparse(call$value),
    snippet_must_be("vector with no NAs"),
    "."
  )

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
#' @concept vector
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
    snippet_must_be("vector containing only NAs"),
    "."
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
#' @concept vector
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

  total_occurrences <- sum(
    eval(call$vec, env) == eval(call$value, env), na.rm = TRUE
  )

  exact_occurrences <- callget(call, env, "exact_occurrences", NULL)
  min_occurrences <- callget(call, env, "min_occurrences", NULL)
  max_occurrences <- callget(call, env, "max_occurrences", NULL)
  value <- callget(call, env, "value", NULL)

  msg <- paste0(
    deparse(call$vec),
    snippet_must_be("vector"),
    " containing value '", value, "'",
    snippet_occurrences(exact_occurrences, min_occurrences, max_occurrences),
    ". Found it ", total_occurrences, " times."
  )
  return(msg)
}
