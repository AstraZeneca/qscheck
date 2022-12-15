#' Check if the passed entity is a single logical value (TRUE or FALSE)
#'
#' @param value the value to check
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
is_logical_value <- function(value) {
  res <- inspect_logical_value(value)

  return(res$valid)
}
assertthat::on_failure(is_logical_value) <- function(call, env) {
  value <- callget(call, env, "value", NULL)

  res <- inspect_logical_value(value)

  return(paste0(
    deparse(call$value),
    snippet_must_be("logical value"),
    ". ", res$reason
  ))
}

inspect_logical_value <- function(value) {
  if (!is.logical(value)) {
    return(failure("Passed value is not a logical"))
  }

  if (length(value) != 1) {
    return(failure(
      "Passed value must be a single logical value, not a vector"
    ))
  }

  return(success())
}
