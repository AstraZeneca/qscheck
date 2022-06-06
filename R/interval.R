#' Check if the two values constitute an interval (low, high)
#'
#' This routine returns False if high < low.
#'
#' @param low the lowest end of the interval
#' @param high the highest end of the interval
#' @param allow_degenerate If TRUE, the case low == high is allowed.
#'        Defaults to TRUE
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_interval(low, high))
#' # For check
#' if (qscheck::is_interval(low, high)) {}
#' }
#'
#' @concept real
#' @export
is_interval <- function(low, high, allow_degenerate = TRUE) {
  res <- .inspect_interval(low, high, allow_degenerate)
  return(res$valid)
}
assertthat::on_failure(is_interval) <- function(call, env) {
  allow_degenerate <- TRUE
  if (!is.null(call$allow_degenerate)) {
    allow_degenerate <- eval(call$allow_degenerate, env)
  }
  res <- .inspect_interval(
    eval(call$low, env), eval(call$high, env), allow_degenerate
  )
  return(.expected_interval_description(call, env, res$reason))
}

.inspect_interval <- function(low, high, allow_degenerate) {
  res <- list(valid = FALSE, reason = "")

  if (!is_real_value(low)) {
    res$reason <- "The low value must be a numeric value"
    return(res)
  }

  if (!is_real_value(high)) {
    res$reason <- "The high value must be a numeric value"
    return(res)
  }

  if (low == high && !allow_degenerate) {
    res$reason <- paste0(
      "The low and high values are degenerate")
    return(res)
  }

  if (low > high) {
    res$reason <- "The low value cannot be higher than the high value"
    return(res)
  }

  res$valid <- TRUE
  return(res)
}

.expected_interval_description <- function(call, env, failure_reason) {

  allow_degenerate <- TRUE
  if (!is.null(call$allow_degenerate)) {
    allow_degenerate <- eval(call$allow_degenerate, env)
  }

  degenerate_msg <- "non-degenerate"
  if (allow_degenerate) {
    degenerate_msg <- "possibly degenerate"
  }

  return(
    paste0(
      "Arguments '", deparse(call$low), "' and '",
      deparse(call$high), "' must define a ", degenerate_msg,
      " numerical interval. ", failure_reason, "."
    )
  )

}
