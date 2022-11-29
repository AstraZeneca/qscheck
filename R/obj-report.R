failure <- function(reason) {
  return(report(FALSE, reason))
}

success <- function() {
  return(report(TRUE, NULL))
}

report <- function(valid, reason = NULL) {
  return(
    structure(
      list(
        valid = valid,
        reason = reason
      )
    ),
    class = "qscheck::report"
  )
}
