#' Checks if the value is a file
#'
#' @param value the value to check
#' @param allow_null if TRUE, allow the value NULL. If FALSE (default) do not
#'                   allow NULL.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_file(f))
#' # For check
#' if (qscheck::is_file(f)) {}
#' }
#'
#'
#' @concept file
#' @export
is_file <- function(
    value, allow_null = FALSE) {

  res <- inspect_is_file(value, allow_null)
  return(res$valid)
}

assertthat::on_failure(is_file) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  res <- inspect_is_file(value, allow_null)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be("file"),
    snippet_null(allow_null),
    ". ", res$reason
  )

  return(msg)
}

inspect_is_file <- function(value, allow_null) {
  if (is.null(value)) {
    if (allow_null) {
      return(success())
    } else {
      return(failure("Passed value cannot be NULL"))
    }
  }

  if (!(file.exists(value))) {
    return(failure(
      paste(
        "Passed value is not a file"
      )
    ))
  }

  return(success())

}
