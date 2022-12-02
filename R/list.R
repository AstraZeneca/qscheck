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

  res <- inspect_list(l,
    required_names = required_names,
    exact_length = exact_length,
    allow_null = allow_null
  )
  return(res$valid)
}
assertthat::on_failure(is_list) <- function(call, env) {
  l <- callget(call, env, "l", NULL)
  required_names <- callget(call, env, "required_names", NULL)
  exact_length <- callget(call, env, "exact_length", NULL)
  allow_null <- callget(call, env, "allow_null", NULL)

  res <- inspect_list(l,
    required_names = required_names,
    exact_length = exact_length,
    allow_null = allow_null
  )

  msg <- paste0(
   deparse(call$l),
   snippet_must_be("list"),
   snippet_length(exact_length),
   snippet_names(required_names),
   snippet_null(allow_null),
   ". ", res$reason
  )
  return(msg)
}

inspect_list <- function(
    l,
    required_names = NULL,
    exact_length = NULL,
    allow_null = FALSE
) {

  if (is.null(l)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is null"))
    }
  }

  if (!inherits(l, "list")) {
    return(failure("The passed entity is not a list"))
  }

  if (!is.null(required_names)) {
    for (name in required_names) {
      if (!(name %in% names(l))) {
        return(failure(
          paste0(
            "Required name '", name, "' is not available in the list"
          )
        ))
      }
    }
  }

  if (!is.null(exact_length)) {
    if (length(l) != exact_length) {
      return(failure(
        paste0(
          "Passed list length is ", length(l),
          " instead of the expected ", exact_length
        )
      ))
    }
  }

  return(success())

}
