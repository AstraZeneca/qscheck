#' Checks if a passed value satisfies one or more tests provided as functions.
#'
#' @param value the value to verify
#' @param func_or_list a function or (optionally named) list of functions.
#'        The function must accept a single argument (which will receive the
#'        value) and return either a logical (TRUE or FALSE), or an instance
#'        of qscheck::report.
#'
#' @examples
#' \dontrun{
#' qscheck::assert(
#'    qscheck::is_satisfying_funcs(
#'      my_parameter,
#'      list(
#'         more_than_five = function(x) {
#'            # Short form. Returns a logical
#'            x > 5
#'         },
#'         less_than_two = function(x) {
#'            # expanded form. use success() and failure(reason)
#'            if (x < 2) {
#'              return(success())
#'            } else {
#'              return(failure("Value must be less than two"))
#'            }
#'         }
#'      )
#'    )
#' ))
#' }
#'
#' @concept general
#' @export
is_satisfying_funcs <- function(value, func_or_list) {
  res <- inspect_is_satisfying_funcs(value, func_or_list)
  return(res$valid)
}
assertthat::on_failure(is_satisfying_funcs) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  func_or_list <- callget(call, env, "func_or_list", NULL)

  res <- inspect_is_satisfying_funcs(value, func_or_list)

  return(
    paste0(
      "Argument '", deparse(call$value),
      "' must satisfy all conditions in the check. ",
      res$reason, "."
    )
  )
}
inspect_is_satisfying_funcs <- function(value, func_or_list) {
  if (inherits(func_or_list, "function")) {
    func_or_list <- list(func_or_list)
  }

  for (func_idx in seq_along(func_or_list)) {
    func <- func_or_list[[func_idx]]
    func_tag <- names(func_or_list)[[func_idx]]
    func_tag <- ifelse(is.null(func_tag), as.character(func_idx), func_tag)

    res <- func(value)
    if (! (is.logical(res) || inherits(res, "qscheck::report"))) {
      return(
        failure(paste0(
          "Check function number ", as.character(func_idx),
          " (tag: ", func_tag, ") did not return a logical or a ",
          "qscheck::report instance."
        )
      ))
    }

    # If it's a logical, convert it to a report instance for uniform handling
    if (is.logical(res)) {
      if (res) {
        res <- success()
      } else {
        res <- failure()
      }
    }

    if (!res$valid) {
      reason <- paste0(
          "Check function number ", as.character(func_idx),
          " (tag: ", func_tag, ") failed"
      )
      if (!is.null(res$reason)) {
        reason <- paste0(reason, ": ", res$reason)
      }
      return(failure(reason = reason, tag = func_tag, subreport = res))
    }
  }

  # We passed all the functions, so return success
  return(success())
}
