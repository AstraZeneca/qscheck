test_that("variableRealValueWithLimits", {
  expect_true(is_real_value(10.0))
  expect_true(is_real_value(-0.1))
  expect_true(is_real_value(0.0, min = 0.0))
  expect_false(is_real_value(0.0, min = 0.0, inclusive_min = FALSE))
  expect_true(is_real_value(0.0, max = 0.0))
  expect_false(is_real_value(0.0, max = 0.0, inclusive_max = FALSE))
  expect_false(is_real_value("hello"))
  expect_false(is_real_value(c(0.0, 0.1)))
  expect_false(is_real_value(NULL))
  expect_true(is_real_value(NULL, allow_null = TRUE))
  expect_false(is_real_value(NA_real_))
  expect_true(is_real_value(NA_real_, allow_na = TRUE))

  foo <- "hello"
  expect_error(
    assertthat::assert_that(is_real_value(foo)),
    "foo must be a real value. Passed value is not a numerical")
})

test_that("variableProbabilityValue", {
  expect_true(is_probability_value(1.0))
  expect_true(is_probability_value(0.5))
  expect_true(is_probability_value(0.0))
  expect_false(is_probability_value(-0.1))
  expect_false(is_probability_value(1.1))
  expect_false(is_probability_value("hello"))
  expect_false(is_probability_value(c(0.0, 0.1)))
  expect_false(is_probability_value(NULL))
  expect_true(is_probability_value(NULL, allow_null = TRUE))

  foo <- -0.3
  expect_error(
    assertthat::assert_that(is_probability_value(foo)),
    paste0(
      "foo must be a probability value in the interval \\[0.0, 1.0\\]. ",
      "Passed value -0.3 is below the minimum of 0"))
})

test_that("variablePositiveRealValue", {
  expect_true(is_positive_real_value(1))
  expect_true(is_positive_real_value(1.01))
  expect_true(is_positive_real_value(NULL, allow_null = TRUE))
  expect_true(is_positive_real_value(NA_real_, allow_na = TRUE))
  expect_false(is_positive_real_value("1"))
  expect_false(is_positive_real_value(c(1.2, 1.3)))

  foo <- -1.5
  expect_error(
    assertthat::assert_that(is_positive_real_value(foo)),
    paste0(
      "foo must be a positive real value. Passed value -1.5 is below ",
      "or equal to the minimum of 0"))

  foo <- -1.5
  expect_error(
    assertthat::assert_that(is_positive_real_value(
        foo, allow_na = TRUE, allow_null = TRUE)),
    paste0(
      "foo must be a positive real value or NA or NULL. ",
      "Passed value -1.5 is below or equal to the minimum of 0"))
})

test_that("variableNonNegativeRealValue", {
  expect_true(is_non_negative_real_value(1))
  expect_true(is_non_negative_real_value(0))
  expect_false(is_non_negative_real_value(-0.01))
  expect_true(is_non_negative_real_value(1.01))
  expect_true(is_non_negative_real_value(NULL, allow_null = TRUE))
  expect_true(is_non_negative_real_value(NA_real_, allow_na = TRUE))
  expect_false(is_non_negative_real_value("1"))
  expect_false(is_non_negative_real_value(c(1.2, 1.3)))

  foo <- -1.5
  expect_error(
    assertthat::assert_that(is_non_negative_real_value(foo)),
    paste0(
      "foo must be a non-negative real value. ",
      "Passed value -1.5 is below the minimum of 0"))

  foo <- -1.5
  expect_error(
    assertthat::assert_that(is_non_negative_real_value(
        foo, allow_na = TRUE, allow_null = TRUE)),
    paste0(
      "foo must be a non-negative real value or NA or NULL. ",
      "Passed value -1.5 is below the minimum of 0"))
})
