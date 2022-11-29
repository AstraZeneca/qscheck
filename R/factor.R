#' Check if the passed entity is a factor.
#'
#' @param value the value to check
#' @param exact_levels vector of strings of the expected levels of the factor.
#'                   The factor must contain *exactly* the specified elements.
#' @param exact_length integer value. If passed, the factor must have the
#'                    *exact* specified length.
#' @param allow_null boolean. if TRUE, NULL is accepted as a valid value.
#'                   If FALSE (default) do not accept it.
#' @param allow_na_values boolean. If passed allows factors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_factor(my_parameter))
#' # For check
#' if (qscheck::is_factor(my_parameter)) {}
#' }
#'
#'
#' @concept factor
#' @export
is_factor <- function(
  value, exact_levels = NULL, exact_length = NULL, allow_null = FALSE,
  allow_na_values = FALSE) {
  if (is.null(value)) {
    return(allow_null)
  }

  if (!is.factor(value)) {
    return(FALSE)
  }
  if (!is.null(exact_length) && length(value) != exact_length) {
    return(FALSE)
  }
  if (!is.null(exact_levels)) {
    if (!all(exact_levels == levels(value))) {
      return(FALSE)
    }
  }
  if (any(is.na(value)) && !allow_na_values) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_factor) <- function(call, env) {
  exact_length <- callget(call, env, "exact_length", NULL)
  exact_levels <- callget(call, env, "exact_levels", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  msg <- paste0(
   deparse(call$value),
   snippet_must_be("factor"),
   snippet_length(exact_length),
   snippet_exact_levels(exact_levels),
   snippet_na_values(allow_na_values),
   ";",
   snippet_null(allow_null),
   ". Got: ",
   deparse(eval(call$value, env))
  )
  return(msg)
}
