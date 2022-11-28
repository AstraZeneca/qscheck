#' Check if the passed entity is a list.
#'
#' @param l the value to check
#' @param required_names vector of strings. If passed, checks if the list
#'                   contains *at least* the specified elements.
#' @param exact_length integer value. If passed, the list must have the
#'                    *exact* specified length
#' @param allow_null if TRUE, NULL is accepted as a valid value.
#'                   If FALSE (default) do not accept it.
#' @details
#' Note that a list with names set to NULL are considered not to be present.
#' This is an R detail, not a qscheck detail.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_list(my_parameter))
#' # For check
#' if (qscheck::is_list(my_parameter)) {}
#' }
#'
#'
#' @concept list
#' @export
is_list <- function(
    l,
    required_names = NULL,
    exact_length = NULL,
    allow_null = FALSE
    ) {

  if (is.null(l) && allow_null) {
    return(TRUE)
  }

  if (!inherits(l, "list")) {
    return(FALSE)
  }
  if (!is.null(required_names)) {
    for (name in required_names) {
      if (!(name %in% names(l))) {
        return(FALSE)
      }
    }
  }

  if (!is.null(exact_length)) {
    return(assertthat::are_equal(length(l), exact_length))
  }
  return(TRUE)
}
assertthat::on_failure(is_list) <- function(call, env) {
  msg <- paste0(deparse(call$l), " must be a list")
  required_names <- callget(call, env, "required_names", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  allow_null <- callget(call, env, "allow_null", NULL)

  if (!is.null(exact_length)) {
    msg <- paste0(msg, " of exact length ", exact_length,
                  " (passed ", length(eval(call$l, env)), ")")
  }

  if (!is.null(required_names)) {
    msg <- paste0(msg, " with at least names '",
                  paste0(
                    required_names,
                    collapse = "', '"
                    ),
                  "'"
                  )
  }

  msg <- paste0(
   msg,
   snippet_null(allow_null),
   ". Got: ",
   deparse(eval(call$l, env))
  )
  return(msg)
}
