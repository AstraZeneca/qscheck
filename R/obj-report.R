failure <- function(reason) {
  return(report(FALSE, reason, TRUE))
}

success <- function(conclusive = FALSE) {
  return(report(TRUE, NULL, conclusive))
}

report <- function(valid, reason = NULL, conclusive = FALSE) {
  return(
    structure(
      list(
        valid = valid,
        reason = reason,
        conclusive = conclusive
      ),
      class = "qscheck::report"
    )
  )
}
