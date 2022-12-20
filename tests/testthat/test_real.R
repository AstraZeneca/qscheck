test_that("is_real_value", {
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

test_that("is_probability_value", {
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

test_that("is_positive_real_value", {
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


test_that("is_non_negative_real_value", {
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


test_that("is_real_vector", {
  v <- c(1.2, 1.3, 3.0)
  v2 <- c("hello", "hi")
  expect_true(is_real_vector(v))
  expect_false(is_real_vector(NULL))
  expect_true(is_real_vector(NULL, allow_null = TRUE))
  expect_false(is_real_vector(v2))
  expect_false(is_real_vector(v, exact_length = 4))
  expect_false(is_real_vector(v, min_length = 4))
  expect_true(is_real_vector(v, max_length = 4))
  expect_false(is_real_vector(v, max_length = 2))
  expect_true(is_real_vector(v, exact_length = 3, max_length = 2))

  v2 <- 1.0
  expect_true(is_real_vector(v2))

  expect_error(
    assertthat::assert_that(is_real_vector(v, exact_length = 4)),
    paste0("v must be a vector of real numbers of exact length 4",
    " with no NAs. Passed vector length is 3 instead of the expected 4"
    ))

  expect_error(
    assertthat::assert_that(is_real_vector(v, min_length = 4)),
    paste0(
      "v must be a vector of real numbers of length not less than 4",
      " with no NAs. Passed vector length is 3 but must be at least 4"
      ))

  expect_error(
    assertthat::assert_that(is_real_vector(v, max_length = 2)),
    paste0(
      "v must be a vector of real numbers of length not ",
      "greater than 2 with no NAs. Passed vector length ",
      "is 3 but must be at most 2"
      ))

  expect_error(
    assertthat::assert_that(is_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE)),
    paste0(
      "v must be a vector of real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4"))
})

test_that("is_positive_real_vector", {
  v <- c(1.2, 1.3, 3.0)
  v2 <- c(1.2, -1.3, 3.0)
  v3 <- c(1.2, 1.3, 0.0)
  v4 <- c("hello", "hi")
  expect_true(is_positive_real_vector(v))
  expect_false(is_positive_real_vector(v2))
  expect_false(is_positive_real_vector(v3))
  expect_false(is_positive_real_vector(v4))
  expect_false(is_positive_real_vector(NULL))
  expect_true(is_positive_real_vector(NULL, allow_null = TRUE))
  expect_false(is_positive_real_vector(v, exact_length = 4))
  expect_false(is_positive_real_vector(v, min_length = 4))
  expect_true(is_positive_real_vector(v, max_length = 4))
  expect_false(is_positive_real_vector(v, max_length = 2))
  expect_true(is_positive_real_vector(v, exact_length = 3, max_length = 2))

  v2 <- 1.0
  expect_true(is_positive_real_vector(v2))

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v, exact_length = 4)),
    paste0("v must be a vector of positive real numbers of exact length 4",
    " with no NAs. Passed vector length is 3 instead of the expected 4"))

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v, min_length = 4)),
    paste0(
      "v must be a vector of positive real numbers of length not less than 4",
      " with no NAs. Passed vector length is 3 but must be at least 4"))

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v, max_length = 2)),
    paste0(
      "v must be a vector of positive real numbers of length not ",
      "greater than 2 with no NAs. Passed vector length is 3 but ",
      "must be at most 2"))

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE)),
    paste0(
      "v must be a vector of positive real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4"))
})

test_that("is_non_negative_real_vector", {
  v <- c(1.2, 1.3, 3.0)
  v2 <- c(1.2, -1.3, 3.0)
  v3 <- c(1.2, 1.3, 0.0)
  v4 <- c("hello", "hi")
  expect_true(is_non_negative_real_vector(v))
  expect_false(is_non_negative_real_vector(v2))
  expect_true(is_non_negative_real_vector(v3))
  expect_false(is_non_negative_real_vector(v4))
  expect_false(is_non_negative_real_vector(NULL))
  expect_true(is_non_negative_real_vector(NULL, allow_null = TRUE))
  expect_false(is_non_negative_real_vector(v, exact_length = 4))
  expect_false(is_non_negative_real_vector(v, min_length = 4))
  expect_true(is_non_negative_real_vector(v, max_length = 4))
  expect_false(is_non_negative_real_vector(v, max_length = 2))
  expect_true(is_non_negative_real_vector(v, exact_length = 3, max_length = 2))

  v2 <- 1.0
  expect_true(is_non_negative_real_vector(v2))

  expect_error(
    assertthat::assert_that(is_non_negative_real_vector(v, exact_length = 4)),
    paste0("v must be a vector of non-negative real numbers of exact length 4",
    " with no NAs. Passed vector length is 3 instead of the expected 4"))

  expect_error(
    assertthat::assert_that(is_non_negative_real_vector(v, min_length = 4)),
    paste0(
      "v must be a vector of non-negative real numbers of length not less than 4",
      " with no NAs. Passed vector length is 3 but must be at least 4"))

  expect_error(
    assertthat::assert_that(is_non_negative_real_vector(v, max_length = 2)),
    paste0(
      "v must be a vector of non-negative real numbers of length not ",
      "greater than 2 with no NAs. Passed vector length is 3 but must ",
      "be at most 2"))

  expect_error(
    assertthat::assert_that(is_non_negative_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE)),
    paste0(
      "v must be a vector of non-negative real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4"))
})

test_that("is_probability_vector", {
  v <- c(0.2, 0.0, 1.0)
  v2 <- c(1.2, -1.3, 3.0)
  v3 <- c(0.2, -0.1, 1.0)
  v4 <- c("hello", "hi")
  expect_true(is_probability_vector(v))
  expect_false(is_probability_vector(v2))
  expect_false(is_probability_vector(v3))
  expect_false(is_probability_vector(v4))
  expect_false(is_probability_vector(NULL))
  expect_true(is_probability_vector(NULL, allow_null = TRUE))
  expect_false(is_probability_vector(v, exact_length = 4))
  expect_false(is_probability_vector(v, min_length = 4))
  expect_true(is_probability_vector(v, max_length = 4))
  expect_false(is_probability_vector(v, max_length = 2))
  expect_true(is_probability_vector(v, exact_length = 3, max_length = 2))

  v2 <- 1.0
  expect_true(is_probability_vector(v2))

  expect_error(
    assertthat::assert_that(is_probability_vector(v, exact_length = 4)),
    paste0(
      "v must be a vector of values in the interval \\[0.0, 1.0\\] ",
      "of exact length 4 with no NAs. Passed vector length is 3 instead ",
      "of the expected 4"))

  expect_error(
    assertthat::assert_that(is_probability_vector(v, min_length = 4)),
    paste0(
      "v must be a vector of values in the interval \\[0.0, 1.0\\] of length not less than 4",
      " with no NAs. Passed vector length is 3 but must be at least 4"
      ))

  expect_error(
    assertthat::assert_that(is_probability_vector(v, max_length = 2)),
    paste0(
      "v must be a vector of values in the interval \\[0.0, 1.0\\] of length not ",
      "greater than 2 with no NAs. Passed vector length is 3 but must be at most 2"
      ))

  expect_error(
    assertthat::assert_that(is_probability_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE)),
    paste0(
      "v must be a vector of values in the interval \\[0.0, 1.0\\] of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4"))
})


test_that("is_real_vector with NA", {
  v <- c(1.2, NA, 3.0)
  expect_false(is_real_vector(v))
  expect_true(is_real_vector(v, allow_na_values = TRUE))
})

test_that("is_increasing_vector", {
  v <- c(1.2, 3.0)
  expect_true(is_increasing_vector(v))

  v <- c(3.0, 1.2)
  expect_false(is_increasing_vector(v))

  v <- c(1.2, NA, 3.0)
  expect_false(is_increasing_vector(v))

  v <- c(1.2, NA, 3.0)
  expect_true(is_increasing_vector(v, allow_na_values = TRUE))

  v <- c(1.2, 1.2, 3.0)
  expect_true(is_increasing_vector(v))

  v <- c(1.2, 1.2, 3.0)
  expect_false(is_increasing_vector(v, strictly = TRUE))

  v <- c(1.2, 1.2, 3.0)
  expect_error(
    assertthat::assert_that(is_increasing_vector(v, strictly = TRUE)),
    "v must be a vector of strictly increasing numbers with no NAs."
  )

  v <- c(3.2, 1.2, 3.0)
  expect_error(
    assertthat::assert_that(is_increasing_vector(v)),
    "v must be a vector of increasing numbers with no NAs."
  )

  v <- c(3.2, 1.2, 3.0)
  expect_error(
    assertthat::assert_that(is_increasing_vector(v, allow_na_values = TRUE)),
    "v must be a vector of increasing numbers or NAs."
  )

})

test_that("is_decreasing_vector", {
  v <- c(3.0, 1.2)
  expect_true(is_decreasing_vector(v))

  v <- c(1.2, 3.0)
  expect_false(is_decreasing_vector(v))

  v <- c(3.0, NA, 1.2)
  expect_false(is_decreasing_vector(v))

  v <- c(3.0, NA, 1.2)
  expect_true(is_decreasing_vector(v, allow_na_values = TRUE))

  v <- c(3.0, 1.2, 1.2)
  expect_true(is_decreasing_vector(v))

  v <- c(3.0, 1.2, 1.2)
  expect_false(is_decreasing_vector(v, strictly = TRUE))

  v <- c(3.0, 1.2, 1.2)
  expect_error(
    assertthat::assert_that(is_decreasing_vector(v, strictly = TRUE)),
    "v must be a vector of strictly decreasing numbers with no NAs."
  )

  v <- c(3.2, 1.2, 3.0)
  expect_error(
    assertthat::assert_that(is_decreasing_vector(v)),
    "v must be a vector of decreasing numbers with no NAs."
  )

  v <- c(3.2, 1.2, 3.0)
  expect_error(
    assertthat::assert_that(is_decreasing_vector(v, allow_na_values = TRUE)),
    "v must be a vector of decreasing numbers or NAs."
  )

})
