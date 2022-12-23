#' Checks if the value is a function
#'
#' @param value the value to check
#' @param num_args an integer. If defined, the function must have
#'             the exact number of arguments.
#' @param args a vector of strings. If defined, the function must have
#'             the exact argument names, in the specified order.
#' @param allow_null if TRUE, allow the value NULL. If FALSE (default) do not
#'                   allow NULL.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_function(f))
#' # For check
#' if (qscheck::is_function(f)) {}
#' }
#'
#'
#' @concept function
#' @export
is_function <- function(
    value, num_args = NULL, args = NULL, allow_null = FALSE) {

  res <- inspect_function(value, num_args, args, allow_null)
  return(res$valid)
}

assertthat::on_failure(is_function) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  num_args <- callget(call, env, "num_args", NULL)
  args <- callget(call, env, "args", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_function(value, num_args, args, allow_null)


  msg <- paste0(
    deparse(call$value),
    snippet_must_be("function"),
    snippet_function_args(num_args, args),
    snippet_null(allow_null),
    ". ", res$reason
  )

  return(msg)
}

inspect_function <- function(value, num_args, args, allow_null) {
  if (is.null(value)) {
    if (allow_null) {
      return(success())
    } else {
      return(failure("Passed value cannot be NULL"))
    }
  }

  if (!inherits(value, "function")) {
    return(failure("Passed value is not a function"))
  }

  func_args <- formals(value)
  if (!is.null(num_args)) {
    if (length(func_args) != num_args) {
      return(failure(
          paste0(
          "Passed function has an incorrect number of arguments (",
          length(func_args), ")"
          ))
      )
    }
  }

  if (!is.null(args)) {
    num_args <- length(args)

    if (num_args != length(func_args)) {
      return(failure(paste0(
        "The passed function has an incorrect number of arguments (",
        length(func_args),
        ")"
      )))
    }

    for (idx in seq_len(num_args)) {
      if (args[idx] != names(func_args)[idx]) {
        return(failure(
          paste0(
            "Argument ", idx, " was '", names(func_args)[idx],
            "' but '", args[idx], "' is required"
          )
        ))
      }
    }
  }

  return(success())
}
