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
#' @export
is_function <- function(
    value, num_args = NULL, args = NULL, allow_null = FALSE) {

  res <- .inspect_function(value, num_args, args, allow_null)

  return(res$valid)
}

assertthat::on_failure(is_function) <- function(call, env) {
  allow_null <- FALSE
  if (!is.null(call$allow_null)) {
    allow_null <- eval(call$allow_null, env)
  }

  args <- NULL
  if (!is.null(call$args)) {
    args <- eval(call$args, env)
  }

  num_args <- NULL
  if (!is.null(call$num_args)) {
    num_args <- eval(call$num_args, env)
  }

  value_name <- deparse(call$value)
  res <- .inspect_function(eval(call$value, env), num_args, args, allow_null)
  msg <- .expected_description(
    value_name, num_args, args, allow_null, res$reason
  )
  return(msg)
}

.inspect_function <- function(value, num_args, args, allow_null) {
  res <- list(
    valid = FALSE,
    reason = ""
  )

  if (is.null(value)) {
    if (allow_null) {
      res$valid <- TRUE
      return(res)
    } else {
      res$reason <- "The passed value was NULL"
    }
  }

  if (!inherits(value, "function")) {
    res$reason <- "The passed value was not a function"
    return(res)
  }

  func_args <- formals(value)
  if (!is.null(num_args)) {
    if (length(func_args) != num_args) {
      res$reason <- paste0(
        "The passed function has an incorrect number of arguments (",
        as.character(length(func_args)),
        ")"
      )
      return(res)
    }
  }

  if (!is.null(args)) {
    num_args <- length(args)

    if (num_args != length(func_args)) {
      res$reason <- paste0(
        "The passed function has an incorrect number of arguments (",
        as.character(length(func_args)),
        ")"
      )
      return(res)
    }

    for (idx in seq_len(num_args)) {
      if (args[idx] != names(func_args)[idx]) {
        res$reason <- paste0(
          "Argument ", as.character(idx), " was '", names(func_args)[idx],
          "' but '", args[idx], "' is required")
        return(res)
      }
    }
  }

  res$valid <- TRUE
  return(res)
}

.expected_description <- function(
    value_name, num_args, args, allow_null, failure_reason) {
  allow_null_msg <- ""
  if (allow_null) {
    allow_null_msg <- " or NULL"
  }

  args_msg <- ""
  if (!is.null(args)) {
    args_msg <- paste0(" with arguments named ", paste(args, collapse = ", "))
  }

  num_args_msg <- ""
  if (!is.null(num_args)) {
    num_args_msg <- paste0(" with ", num_args, " arguments")
  }

  msg <- paste0(
    value_name,
    " must be a function",
    num_args_msg,
    args_msg,
    allow_null_msg,
    ". ",
    failure_reason,
    ".")

  return(msg)
}
