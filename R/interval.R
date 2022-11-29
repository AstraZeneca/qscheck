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
  low <- callget(call, env, "low", NULL)
  high <- callget(call, env, "high", NULL)
  allow_degenerate <- callget(call, env, "allow_degenerate", TRUE)

  res <- .inspect_interval(low, high, allow_degenerate)

  return(
    paste0(
      "Arguments '", deparse(call$low), "' and '", deparse(call$high),
      "' must define a",
      snippet_degenerate(allow_degenerate),
      " numerical interval. ", res$reason, "."
    )
  )
}

.inspect_interval <- function(low, high, allow_degenerate) {
  if (!is_real_value(low)) {
    return(failure("The low value must be a numeric value"))
  }

  if (!is_real_value(high)) {
    return(failure("The high value must be a numeric value"))
  }

  if (low == high && !allow_degenerate) {
    return(failure("The low and high values are degenerate"))
  }

  if (low > high) {
    return(failure("The low value cannot be higher than the high value"))
  }

  return(success())
}
