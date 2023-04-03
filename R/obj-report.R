failure <- function(reason = NULL, tag = NULL, subreport = NULL) {
  return(report(FALSE, reason, tag, subreport))
}

success <- function() {
  return(report(TRUE, NULL, NULL))
}

report <- function(valid, reason = NULL, tag = NULL, subreport = NULL) {
  return(
    structure(
      list(
        valid = valid,
        reason = reason,
        tag = tag,
        subreport = subreport
      ),
      class = "qscheck::report"
    )
  )
}
