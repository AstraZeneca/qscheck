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

  res <- inspect_factor(
    value,
    exact_levels = exact_levels,
    exact_length = exact_length,
    allow_null = allow_null,
    allow_na_values = allow_na_values
  )

  return(res$valid)
}
assertthat::on_failure(is_factor) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  exact_levels <- callget(call, env, "exact_levels", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  allow_na_values <- callget(call, env, "allow_na_values", FALSE)

  res <- inspect_factor(value,
    exact_levels = exact_levels,
    exact_length = exact_length,
    allow_null = allow_null,
    allow_na_values = allow_na_values
  )

  msg <- paste0(
   deparse(call$value),
   snippet_must_be("factor"),
   snippet_length(exact_length),
   snippet_exact_levels(exact_levels),
   snippet_na_values(allow_na_values),
   ";",
   snippet_null(allow_null),
   ". ", res$reason
  )
  return(msg)
}

inspect_factor <- function(
    value, exact_levels = NULL, exact_length = NULL, allow_null = FALSE,
    allow_na_values = FALSE) {


  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("passed value is NULL"))
    }
  }

  if (!is.factor(value)) {
    return(failure("passed value is not a factor"))
  }

  if (!is.null(exact_length) && length(value) != exact_length) {
    return(failure(
      paste0(
        "factor length of ", length(value), " does not match the ",
        "expected exact length of ", exact_length
      )
    ))
  }
  if (!is.null(exact_levels)) {
    if (!all(exact_levels == levels(value))) {
      return(failure(
        paste0(
          "Passed factor has levels ", flatten_vector(levels(value)),
          "which do not match the exact expected levels ",
          flatten_vector(exact_levels)
        )
      ))
    }
  }
  if (any(is.na(value)) && !allow_na_values) {
    return(failure(
      paste0("factor contains NAs")
    ))
  }

  return(success())
}
