
check_limits <- function(value, min, max, inclusive_min, inclusive_max) {
  if (!is.null(min)) {
    if (inclusive_min) {
      if (value < min) {
        return(failure(
          paste0(
            "Passed value ", value, " is below the minimum of ", min)
          )
        )
      }
    } else {
      if (value <= min) {
        return(failure(
          paste0(
            "Passed value ", value,
            " is below or equal to the minimum of ", min)
          )
        )
      }
    }
  }

  if (!is.null(max)) {
    if (inclusive_max) {
      if (value > max) {
        return(failure(
          paste0(
            "Passed value ", value, " is above the maximum of ", max)
          )
        )
      }
    } else {
      if (value >= max) {
        return(failure(
          paste0(
            "Passed value ", value,
            " is above or equal to the maximum of ", max)
          )
        )
      }
    }
  }

  return(success())
}
