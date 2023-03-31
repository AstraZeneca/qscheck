is_satisfying <- function(entity, func_or_list) {
  res <- inspect_is_satisfying(entity, func_or_list)
  return(res$valid)
}
assertthat::on_failure(is_satisfying) <- function(call, env) {
  entity <- callget(call, env, "entity", NULL)
  func_or_list <- callget(call, env, "func_or_list", NULL)

  res <- inspect_is_satisfying(entity, func_or_list)

  return(
    paste0(
      "Argument '", deparse(call$entity),
      "' must satisfy all conditions in the check. ",
      res$reason, "."
    )
  )
}
inspect_is_satisfying <- function(entity, func_or_list) {
  if (inherits(func_or_list, "function")) {
    func_or_list <- list(func_or_list)
  }

  for (func_idx in seq_along(func_or_list)) {
    func <- func_or_list[[func_idx]]
    func_tag <- names(func_or_list)[[func_idx]]
    func_tag <- ifelse(is.null(func_tag), as.character(func_idx), func_tag)

    res <- func(entity)
    if (!is.logical(res)) {
      return(
        failure(paste0(
          "Check function number ", as.character(func_idx),
          " (tag: ", func_tag, ") did not return a logical"
        )
      ))
    }
    if (!res) {
      return(failure(
        paste0(
          "Check function number ", as.character(func_idx),
          " (tag: ", func_tag, ") failed"
        ),
        tag = func_tag
        )
      )
    }
  }
  return(success())
}
