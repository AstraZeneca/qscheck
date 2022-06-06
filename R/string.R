#' Checks if the passed value is a single string.
#'
#' @param value the value to verify
#' @param allow_empty If TRUE (default) accept an empty string. If FALSE, don't
#' @param allow_na If TRUE, accepts a single NA value. If FALSE (default) don't
#' @param allow_null If TRUE, accepts a single NULL value.
#'                   If FALSE (default) don't
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_string_value(my_parameter))
#' # For check
#' if (qscheck::is_string_value(my_parameter)) {}
#' }
#'
#' @concept string
#' @export
is_string_value <- function(value, allow_empty = TRUE,
                      allow_na = FALSE, allow_null = FALSE) {
  if (is_na_value(value) && allow_na) {
    return(TRUE)
  }

  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!assertthat::is.string(value)) {
    return(FALSE)
  }

  if (nchar(value) == 0) {
    if (allow_empty) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_string_value) <- function(call, env) {
  non_empty_msg <- ""

  if (!is.null(call$allow_empty)) {
    if (!eval(call$allow_empty, env)) {
      non_empty_msg <- " non-empty"
    }
  }

  allow_na_msg <- ""
  if (!is.null(call$allow_na)) {
    if (eval(call$allow_na, env)) {
      allow_na_msg <- " or NA"
    }
  }

  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  msg <- paste0(
    deparse(call$value),
    " must be a",
    non_empty_msg,
    " string",
    allow_na_msg,
    allow_null_msg,
    ". Got: ",
    deparse(eval(call$value, env))
    )
  return(msg)
}
