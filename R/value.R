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
#' @concept na
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

#' Checks if a value is a single NA of real type
#'
#' @description
#' Allows to check if a given value is a single NA value of type real
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' assertthat::assert_that(
#'   qscheck::is_na_real_value(value)
#' )
#' }
#'
#' @concept na
#' @export
is_na_real_value <- function(value) {
  res <- inspect_na_typed_value(value, "numeric")
  return(res$valid)
}
assertthat::on_failure(is_na_real_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  res <- inspect_na_typed_value(value, "numeric")

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("NA_real_"),
    ". ", res$reason)
  return(msg)
}

#' Checks if a value is a single NA of character type
#'
#' @description
#' Allows to check if a given value is a single NA value of type character
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' assertthat::assert_that(
#'   qscheck::is_na_character_value(value)
#' )
#' }
#'
#' @concept na
#' @export
is_na_character_value <- function(value) {
  res <- inspect_na_typed_value(value, "character")
  return(res$valid)
}
assertthat::on_failure(is_na_character_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  res <- inspect_na_typed_value(value, "character")

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("NA_character_"),
    ". ", res$reason)
  return(msg)
}

#' Checks if a value is a single NA of logical type
#'
#' @description
#' Allows to check if a given value is a single NA value of type logical
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' assertthat::assert_that(
#'   qscheck::is_na_logical_value(value)
#' )
#' }
#'
#' @concept na
#' @export
is_na_logical_value <- function(value) {
  res <- inspect_na_typed_value(value, "logical")
  return(res$valid)
}
assertthat::on_failure(is_na_logical_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  res <- inspect_na_typed_value(value, "logical")

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("logical NA"),
    ". ", res$reason)
  return(msg)
}
inspect_na_typed_value <- function(value, type) {
  res <- inspect_na_value(value)
  if (!res$valid) {
    return(res)
  }

  check_fn <- switch(
    type,
    logical = is.logical,
    character = is.character,
    numeric = is.numeric
  )
  if (!check_fn(value)) {
    return(failure(
      paste0(
        "Passed value is NA of incorrect type ", class(value)
      )
    ))
  }

  return(success())

}
