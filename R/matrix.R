#' Check if the passed entity is a matrix
#'
#' @param value the value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_matrix(value))
#' # For check
#' if (qscheck::is_matrix(value)) {}
#' }
#'
#' @concept matrix
#' @export
is_matrix <- function(
    value, exact_num_rows = NULL, exact_num_cols = NULL, allow_null = FALSE) {
  res <- inspect_matrix(
    value, exact_num_rows, exact_num_cols, allow_null
  )
  return(res$valid)
}
assertthat::on_failure(is_matrix) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_num_rows <- callget(call, env, "exact_num_rows", NULL)
  exact_num_cols <- callget(call, env, "exact_num_cols", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_matrix(
    value, exact_num_rows, exact_num_cols, allow_null
  )
  return(paste0(
    deparse(call$value),
    snippet_must_be("matrix"),
    snippet_exact_matrix_dimension(exact_num_rows, exact_num_cols),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}

inspect_matrix <- function(
  value, exact_num_rows = NULL, exact_num_cols = NULL, allow_null = FALSE) {

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (!is_s3_instance(value, "matrix")) {
    return(failure("Passed value is not a matrix"))
  }

  if (!is.null(exact_num_rows) && nrow(value) != exact_num_rows) {
    return(failure(
      paste0("Passed matrix has ", nrow(value), " rows")
    ))
  }

  if (!is.null(exact_num_cols) && ncol(value) != exact_num_cols) {
    return(failure(
      paste0("Passed matrix has ", ncol(value), " columns")
    ))
  }

  return(success())
}
