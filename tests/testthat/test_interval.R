test_that("twoValuesOrderedIntervalLimits", {

  expect_true(is_interval(0, 1))
  expect_true(is_interval(1, 1, allow_degenerate = TRUE))

  err <- tryCatch({
    assertthat::assert_that(is_interval(1, 0))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: Arguments '1' and '0' must define a possibly degenerate ",
      "numerical interval. The low value cannot be higher than the high ",
      "value.\n"
    )
  )

  err <- tryCatch({
    assertthat::assert_that(is_interval("X", 0))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: Arguments '\"X\"' and '0' must define a possibly degenerate ",
      "numerical interval. The low value must be a numeric value.\n"
    )
  )

  err <- tryCatch({
    assertthat::assert_that(is_interval(0, "X"))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: Arguments '0' and '\"X\"' must define a possibly degenerate ",
      "numerical interval. The high value must be a numeric value.\n"
    )
  )

  err <- tryCatch({
    assertthat::assert_that(is_interval(1, 0, allow_degenerate = TRUE))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: Arguments '1' and '0' must define a possibly degenerate ",
      "numerical interval. The low value cannot be higher than the high ",
      "value.\n"
    )
  )

  err <- tryCatch({
    assertthat::assert_that(is_interval(1, 0, allow_degenerate = FALSE))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: Arguments '1' and '0' must define a non-degenerate ",
      "numerical interval. The low value cannot be higher than the ",
      "high value.\n"
    )
  )

  err <- tryCatch({
    assertthat::assert_that(is_interval(1, 1, allow_degenerate = FALSE))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: Arguments '1' and '1' must define a non-degenerate ",
      "numerical interval. The low and high values are degenerate.\n"
    )
  )

})
