#' Check if the passed entity is a single logical value (TRUE or FALSE)
#'
#' @param value the value to check
#' @param allow_na if TRUE, accepts a NA value
#' @param allow_null if TRUE, accepts a null value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_logical_value(value))
#' # For check
#' if (qscheck::is_logical_value(value)) {}
#' }
#'
#' @concept logical
#' @export
is_logical_value <- function(value,
    allow_na = FALSE, allow_null = FALSE
  ) {
  res <- inspect_logical_value(value, allow_na, allow_null)

  return(res$valid)
}
assertthat::on_failure(is_logical_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_logical_value(value, allow_na, allow_null)

  return(paste0(
    deparse(call$value),
    snippet_must_be("logical value"),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}

inspect_logical_value <- function(value, allow_na, allow_null) {
  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (!is.logical(value)) {
    return(failure("Passed value is not a logical"))
  }

  if (length(value) != 1) {
    return(failure(
      "Passed value must be a single logical value, not a vector"
    ))
  }

  if (is_na_value(value)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NA"))
    }
  }

  return(success())
}
