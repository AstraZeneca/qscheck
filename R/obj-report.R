#' Function to create a misconfiguration. Returns a qscheck::report object.
#'
#' @param reason A string explaining the reason for the misconfiguration
#         to the user.
#'
#' @export
misconfiguration <- function(reason = NULL) {
  return(report(FALSE, reason = reason, misconfiguration = TRUE))
}
#' Function to create a failure. Returns a qscheck::report object.
#'
#' @param reason A string explaining the reason for the failure to the user.
#' @param tag A unique tag to identify the failure
#' @param subreport if the failure arises from a subcheck, this will contain
#'        the report of that subcheck
#'
#' @export
failure <- function(
    reason = NULL, tag = NULL, subreport = NULL) {
  return(report(FALSE, reason, tag, subreport))
}

#' Function to create a success. Returns a qscheck::report object.
#'
#' @export
success <- function() {
  return(report(TRUE, NULL, NULL))
}

report <- function(
    valid, reason = NULL, tag = NULL, subreport = NULL,
    misconfiguration = FALSE) {
  return(
    structure(
      list(
        valid = valid,
        reason = reason,
        tag = tag,
        subreport = subreport,
        misconfiguration = misconfiguration
      ),
      class = "qscheck::report"
    )
  )
}
