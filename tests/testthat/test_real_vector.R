test_that("variableVectorRealValues", {
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

test_that("variableVectorRealValuesMinMax", {
  v <- c(1.2, 1.3, 3.0)
  expect_true(vector_values_between(v, min = 1.2, max = Inf))
  expect_false(vector_values_between(v, min = 1.2, max = Inf, inclusive_min = FALSE))
  expect_true(vector_values_between(v, min = -Inf, max = 3.0))
  expect_false(vector_values_between(v, min = -Inf, max = 3.0, inclusive_max = FALSE))

  expect_error(
    assertthat::assert_that(
      vector_values_between(v, min = 2.0, max = Inf, inclusive_min = TRUE)
    ),
    paste0(
      "v must be a vector of values in the range \\[2, Inf\\) ",
      "with no NAs. Values at positions 1, 2 are not complying with the requirement"
    )
  )

})

test_that("variableVectorRealValuesMinMaxNAValues", {
  v <- c(1.2, 1.3, NA, 3.0)
  expect_true(vector_values_between(v, min = 1.2, max = Inf, allow_na_values = TRUE))
  expect_false(vector_values_between(v, min = 1.2, max = Inf))
  expect_error(
    assertthat::assert_that(
      vector_values_between(v, min = 2.0, max = Inf)
    ),
    paste0(
      "v must be a vector of values in the range \\[2, Inf\\) with no NAs\\. ",
      "Vector contains NA values, but they are not allowed"
    )
  )
})


test_that("variableVectorPositiveReals", {
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
    " with no NAs. Passed vector length is 3 instead of the expected 4"
    )
  )

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v, min_length = 4)),
    paste0(
      "v must be a vector of positive real numbers of length not less than 4",
      " with no NAs. Passed vector length is 3 but must be at least 4"
    )
  )

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v, max_length = 2)),
    paste0(
      "v must be a vector of positive real numbers of length not ",
      "greater than 2 with no NAs. Passed vector length is 3 but ",
      "must be at most 2"
    )
  )

  expect_error(
    assertthat::assert_that(is_positive_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE)),
    paste0(
      "v must be a vector of positive real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4"
    )
  )
})

test_that("variableVectorPositiveRealsMinMax", {
  v <- c(1.2, 1.3, 3.0)
  expect_true(vector_values_between(v, min = 1.2, max = Inf))
  expect_false(vector_values_between(v, min = 1.2, max = Inf, inclusive_min = FALSE))
  expect_true(vector_values_between(v, min = -Inf, max = 3.0))
  expect_false(vector_values_between(v, min = -Inf, max = 3.0, inclusive_max = FALSE))

  expect_error(
    assertthat::assert_that(
      vector_values_between(v, min = 2.0, max = Inf, inclusive_min = TRUE)),
    paste0(
      "v must be a vector of values in the range \\[2, Inf\\) ",
      "with no NAs\\. Values at positions 1, 2 are not complying with the requirement"
    )
  )

})

test_that("variableVectorNonNegativeReals", {
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

test_that("variableVectorNonNegativeRealsMinMax", {
  v <- c(0, 1.3, 3.0)
  expect_true(vector_values_between(v, min = 0, max = Inf))
  expect_false(vector_values_between(v, min = 0, max = Inf, inclusive_min = FALSE))
  expect_true(vector_values_between(v, min = -Inf, max = 3.0))
  expect_false(vector_values_between(v, min = -Inf, max = 3.0, inclusive_max = FALSE))

  expect_error(
    assertthat::assert_that(
      vector_values_between(v, min = 2.0, max = Inf, inclusive_min = TRUE)),
    paste0(
      "v must be a vector of values in the range \\[2, Inf\\) ",
      "with no NAs\\. Values at positions 1, 2 are not complying with the requirement"
      )
    )

})

test_that("variableVectorProbabilities", {
  v <- c(0.2, 0.0, 1.0)
  v2 <- c(1.2, -1.3, 3.0)
  v3 <- c(0.2, -0.1, 1.0)
  v4 <- c("hello", "hi")
  v5 <- 1.0
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
  expect_true(is_probability_vector(v5))

  expect_error(
    assertthat::assert_that(is_probability_vector(v, exact_length = 4)),
    paste0(
      "v must be a vector of probability values in the interval \\[0.0, 1.0\\] ",
      "of exact length 4 with no NAs. Passed vector length is 3 instead ",
      "of the expected 4"))

  expect_error(
    assertthat::assert_that(is_probability_vector(v, min_length = 4)),
    paste0(
      "v must be a vector of probability values in the interval ",
      "\\[0.0, 1.0\\] of length not less than 4",
      " with no NAs. Passed vector length is 3 but must be at least 4"
      ))

  expect_error(
    assertthat::assert_that(is_probability_vector(v, max_length = 2)),
    paste0(
      "v must be a vector of probability values in the interval ",
      "\\[0.0, 1.0\\] of length not ",
      "greater than 2 with no NAs\\. Passed vector length is 3 but must be at most 2"
      ))

  expect_error(
    assertthat::assert_that(is_probability_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE)),
    paste0(
      "v must be a vector of probability values in the interval ",
      "\\[0.0, 1.0\\] of length ",
      "between 4 and 8 inclusive with no NAs\\. ",
      "Passed vector length is 3 but must be at least 4"))

  v <- c(1.2, NA, 3.0)
  expect_false(is_real_vector(v))
  expect_true(is_real_vector(v, allow_na_values = TRUE))
})

test_that("variableVectorProbabilitiesMinMax", {

  v <- c(0.2, 0.0, 1.0)
  v2 <- c(1.2, -1.3, 3.0)
  v3 <- c(0.2, 0.3, 3.0)
  v4 <- c(0.2, NA, 3.0)

  expect_true(is_probability_vector(v))
  expect_false(is_probability_vector(v2))

  expect_error(
    assertthat::assert_that(
      is_probability_vector(v2)
    ),
    paste0(
      "v2 must be a vector of probability values in the interval \\[0.0, 1.0\\] ",
      "with no NAs. Values at positions 1, 2, 3 are outside the allowed ",
      "range"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_probability_vector(v3)
    ),
    paste0(
      "v3 must be a vector of probability values in the interval \\[0.0, 1.0\\] ",
      "with no NAs. Value at position 3 is outside the allowed ",
      "range"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_probability_vector(v4, allow_na_values = TRUE)
    ),
    paste0(
      "v4 must be a vector of probability values in the interval ",
      "\\[0.0, 1.0\\] or NAs. ",
      "Value at position 3 is outside the allowed range"
    )
  )

})

test_that("vectorsIncreasing", {
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

test_that("vectorsDecreasing", {
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
