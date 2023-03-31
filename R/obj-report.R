failure <- function(reason, tag = NULL) {
  return(report(FALSE, reason, tag))
}

success <- function() {
  return(report(TRUE, NULL, NULL))
}

report <- function(valid, reason = NULL, tag = NULL) {
  return(
    structure(
      list(
        valid = valid,
        reason = reason,
        tag = tag
      ),
      class = "qscheck::report"
    )
  )
}
