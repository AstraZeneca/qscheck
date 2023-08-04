test_that("variableIntegerValueWithinSpecifiedLimits", {
  expect_true(is_integer_value(1))
  expect_false(is_integer_value(1.01))
  expect_false(is_integer_value("1"))
  expect_false(is_integer_value(c(3, 4)))
  expect_true(is_integer_value(1, min = 1))
  expect_true(is_integer_value(1, max = 1))
  expect_true(is_integer_value(2, min = 1, max = 3))
  expect_false(is_integer_value(5, min = 1, max = 3))
  expect_false(is_integer_value(0, min = 1, max = 3))
  expect_true(is_integer_value(NULL, allow_null = TRUE))

  foo <- 2
  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, min = 3))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: foo must be an integer value in the range [3, Inf). ",
      "Passed value 2 must be greater than the minimum value 3\n")
    )

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, max = 1))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: foo must be an integer value in the range (-Inf, 1]. ",
      "Passed value 2 must be less than the maximum value 1\n"
    )
  )

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, min = 3, max = 5))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: foo must be an integer value in the range [3, 5]. ",
      "Passed value 2 must be greater than the minimum value 3\n"
    )
  )

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(data.frame(), min = 3, max = 5))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: data.frame() must be an integer value in the range [3, 5].",
      " Passed value is not a numerical\n")
  )

})

test_that("variablePositiveIntegerValue", {
  expect_true(is_positive_integer_value(3))
  expect_true(is_positive_integer_value(3.0))
  expect_false(is_positive_integer_value(3.5))
  expect_false(is_positive_integer_value(0))
  expect_false(is_positive_integer_value(c(3, 4)))
  expect_false(is_positive_integer_value(-3))
  expect_false(is_positive_integer_value("hello"))
  expect_false(is_positive_integer_value(NULL))
  expect_true(is_positive_integer_value(NULL, allow_null = TRUE))

  err <- tryCatch({
      assertthat::assert_that(is_positive_integer_value(-1))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: -1 must be a positive integer value. ",
      "Passed value -1 must be greater than the minimum value 1\n"
    )
  )
})

test_that("variableNonNegativeIntegerValue", {
  expect_true(is_non_negative_integer_value(3))
  expect_true(is_non_negative_integer_value(3.0))
  expect_true(is_non_negative_integer_value(0))
  expect_false(is_non_negative_integer_value(3.5))
  expect_false(is_non_negative_integer_value(c(3, 4)))
  expect_false(is_non_negative_integer_value(-3))
  expect_false(is_non_negative_integer_value("hello"))
  expect_false(is_non_negative_integer_value(NULL))
  expect_true(is_non_negative_integer_value(NULL, allow_null = TRUE))
  err <- tryCatch({
      assertthat::assert_that(is_non_negative_integer_value(-1))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: -1 must be a non negative integer value. ",
      "Passed value -1 must be greater than the minimum value 0\n"
    )
  )
})
