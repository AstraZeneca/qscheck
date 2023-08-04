test_that("variableIntegerValueNAAccepted", {
  expect_false(is_integer_value(NA))
  expect_false(is_integer_value(NA_real_))
  expect_false(is_integer_value(NA_character_, allow_na = TRUE))
  expect_true(is_integer_value(NA_real_, allow_na = TRUE))

  expect_error(
    assertthat::assert_that(is_integer_value(NA_character_, allow_na = TRUE)),
    paste(
      "NA_character_ must be an integer value or NA\\.",
      "Passed value is not a numerical"
    )
  )
  expect_error(
    assertthat::assert_that(is_integer_value(NA_real_)),
    "NA_real_ must be an integer value\\. Passed value is NA"
  )
})

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
  expect_error(
    assertthat::assert_that(is_integer_value(foo, min = 3)),
    paste0(
      "foo must be an integer value in the range \\[3, Inf\\)\\. ",
      "Passed value 2 must be greater than the minimum value 3")
    )

  expect_error(
    assertthat::assert_that(is_integer_value(foo, max = 1)),
    paste0(
      "foo must be an integer value in the range \\(-Inf, 1\\]\\. ",
      "Passed value 2 must be less than the maximum value 1"
    )
  )

  expect_error(
    assertthat::assert_that(is_integer_value(foo, min = 3, max = 5)),
    paste0(
      "foo must be an integer value in the range \\[3, 5\\]\\. ",
      "Passed value 2 must be greater than the minimum value 3"
    )
  )

  expect_error(
    assertthat::assert_that(is_integer_value(data.frame(), min = 3, max = 5)),
    paste0(
      "data\\.frame\\(\\) must be an integer value in the range \\[3, 5\\]\\.",
      " Passed value is not a numerical")
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

  expect_error(
    assertthat::assert_that(is_positive_integer_value(-1)),
    paste0(
      "-1 must be a positive integer value\\. ",
      "Passed value -1 must be greater than the minimum value 1"
    )
  )
})

test_that("variablePositiveIntegerValueNAAccepted", {
  expect_false(is_positive_integer_value(NA))
  expect_false(is_positive_integer_value(NA_real_))
  expect_false(is_positive_integer_value(NA_character_, allow_na = TRUE))
  expect_true(is_positive_integer_value(NA_real_, allow_na = TRUE))

  expect_error(
    assertthat::assert_that(
      is_positive_integer_value(NA_character_, allow_na = TRUE)),
    paste(
      "NA_character_ must be a positive integer value or NA\\.",
      "Passed value is not a numerical"
    )
  )
  expect_error(
    assertthat::assert_that(is_positive_integer_value(NA_real_)),
    "NA_real_ must be a positive integer value\\. Passed value is NA"
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

  expect_error(
    assertthat::assert_that(is_non_negative_integer_value(-1)),
    paste0(
      "-1 must be a non negative integer value\\. ",
      "Passed value -1 must be greater than the minimum value 0"
    )
  )
})

test_that("variableNonNegativeIntegerValueNAAccepted", {
  expect_false(is_non_negative_integer_value(NA))
  expect_false(is_non_negative_integer_value(NA_real_))
  expect_false(is_non_negative_integer_value(NA_character_, allow_na = TRUE))
  expect_true(is_non_negative_integer_value(NA_real_, allow_na = TRUE))

  expect_error(
    assertthat::assert_that(
      is_non_negative_integer_value(NA_character_, allow_na = TRUE)),
    paste(
      "NA_character_ must be a non negative integer value or NA\\.",
      "Passed value is not a numerical"
    )
  )
  expect_error(
    assertthat::assert_that(is_non_negative_integer_value(NA_real_)),
    "NA_real_ must be a non negative integer value\\. Passed value is NA"
  )
})
