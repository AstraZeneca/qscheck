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
#' @export
is_logical_value <- function(value) {
  return(is.logical(value) && length(value) == 1)
}
assertthat::on_failure(is_logical_value) <- function(call, env) {
  return(paste0(
    deparse(call$value),
    " must be a single logical value. Got: ",
    deparse(eval(call$value, env))
  ))
}
