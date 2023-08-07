#' Function to raise a misconfiguration exception if there's an error.
#'
#' @param param_name The name of the parameter under check.
#' @param check_flag Raise the error if false, otherwise do nothing.
#' @param reason A string explaining the reason for the misconfiguration
#         to the user.
#'
#' @noRd
check_param <- function(param_name, check_flag, reason) {
  if (check_flag) {
    return()
  }
  stop(paste0(
    "Incorrect invocation. Parameter '", param_name, "' is invalid: ",
    reason))
}
