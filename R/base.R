is_satisfying_funcs <- function(entity, func_or_list) {
  res <- inspect_is_satisfying_funcs(entity, func_or_list)
  return(res$valid)
}
assertthat::on_failure(is_satisfying_funcs) <- function(call, env) {
  entity <- callget(call, env, "entity", NULL)
  func_or_list <- callget(call, env, "func_or_list", NULL)

  res <- inspect_is_satisfying_funcs(entity, func_or_list)

  return(
    paste0(
      "Argument '", deparse(call$entity),
      "' must satisfy all conditions in the check. ",
      res$reason, "."
    )
  )
}
inspect_is_satisfying_funcs <- function(entity, func_or_list) {
  if (inherits(func_or_list, "function")) {
    func_or_list <- list(func_or_list)
  }

  for (func_idx in seq_along(func_or_list)) {
    func <- func_or_list[[func_idx]]
    func_tag <- names(func_or_list)[[func_idx]]
    func_tag <- ifelse(is.null(func_tag), as.character(func_idx), func_tag)

    res <- func(entity)
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
