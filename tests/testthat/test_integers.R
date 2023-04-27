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
      "Error: foo must be an integer value in the range [3, inf). ",
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
      "Error: foo must be an integer value in the range (-inf, 1]. ",
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


test_that("variableVectorNonNegativeIntegers", {
  v <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)

  expect_true(is_non_negative_integer_vector(v))
  expect_false(is_non_negative_integer_vector(NULL))
  expect_false(is_non_negative_integer_vector(v2))
  expect_false(is_non_negative_integer_vector(v3))

  expect_false(is_non_negative_integer_vector(v, exact_length = 4))
  expect_false(is_non_negative_integer_vector(v, min_length = 4))
  expect_true(is_non_negative_integer_vector(v, max_length = 4))
  expect_false(is_non_negative_integer_vector(v, max_length = 2))
  expect_true(is_non_negative_integer_vector(
    v, exact_length = 3, max_length = 2))

  v4 <- c(1, NA, 0)

  expect_false(is_non_negative_integer_vector(v4))
  expect_true(is_non_negative_integer_vector(v4, allow_na_values = TRUE))

  err <- tryCatch({
    assertthat::assert_that(
      is_non_negative_integer_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer values of exact ",
      "length 4 with no NAs. Passed vector length is 3 instead of the ",
      "expected 4\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_integer_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer values of length ",
      "not less than 4 with no NAs. Passed vector length is 3 but must be ",
      "at least 4\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_integer_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer values of ",
      "length not greater than 2 with no NAs. Passed vector length ",
      "is 3 but must be at most 2\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_integer_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer values of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4\n"))
})

test_that("variableVectorPositiveIntegers", {
  v <- c(1, 13, 1)
  v1 <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)

  expect_true(is_positive_integer_vector(v))
  expect_false(is_positive_integer_vector(v1))
  expect_false(is_positive_integer_vector(NULL))
  expect_false(is_positive_integer_vector(v2))
  expect_false(is_positive_integer_vector(v3))
  expect_true(is_positive_integer_vector(v, min = 1))
  expect_false(is_positive_integer_vector(v, min = 1, inclusive_min = FALSE))
  expect_true(is_positive_integer_vector(v, max = 13))
  expect_false(is_positive_integer_vector(v, max = 13, inclusive_max = FALSE))
  expect_false(is_positive_integer_vector(v, exact_length = 4))
  expect_false(is_positive_integer_vector(v, min_length = 4))
  expect_true(is_positive_integer_vector(v, max_length = 4))
  expect_false(is_positive_integer_vector(v, max_length = 2))
  expect_true(is_positive_integer_vector(
    v, exact_length = 3, max_length = 2))

  err <- tryCatch({
    assertthat::assert_that(
      is_positive_integer_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0(
      "Error: v must be a vector of positive integer values of exact ",
      "length 4 with no NAs. Passed vector length is 3 instead of the ",
      "expected 4\n"))

  err <- tryCatch({
    assertthat::assert_that(is_positive_integer_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of positive integer values of length ",
      "not less than 4 with no NAs. Passed vector length is 3 but must ",
      "be at least 4\n"))

  err <- tryCatch({
    assertthat::assert_that(is_positive_integer_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of positive integer values of ",
      "length not greater than 2 with no NAs. Passed vector length ",
      "is 3 but must be at most 2\n"))

  err <- tryCatch({
    assertthat::assert_that(is_positive_integer_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of positive integer values of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4\n"))

  v <- c(1, NA, 20)

  expect_false(is_positive_integer_vector(v))
  expect_true(is_positive_integer_vector(v, allow_na_values = TRUE))

})

test_that("variableVectorOfIntegerValues", {
  v <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)
  v4 <- c(3, NA, 3)

  expect_true(is_integer_vector(v))
  expect_false(is_integer_vector(NULL))
  expect_true(is_integer_vector(NULL, allow_null = TRUE))
  expect_false(is_integer_vector(v2))
  expect_true(is_integer_vector(v3))
  expect_true(is_integer_vector(v, min = 0))
  expect_false(is_integer_vector(v, min = 0, inclusive_min = FALSE))
  expect_true(is_integer_vector(v, max = 13))
  expect_false(is_integer_vector(v, max = 13, inclusive_max = FALSE))
  expect_false(is_integer_vector(v, exact_length = 4))
  expect_false(is_integer_vector(v, min_length = 4))
  expect_true(is_integer_vector(v, max_length = 4))
  expect_false(is_integer_vector(v, max_length = 2))
  expect_true(is_integer_vector(
    v, exact_length = 3, max_length = 2))

  expect_true(is_integer_vector(v4, allow_na_values = TRUE))
  expect_false(is_integer_vector(v4, allow_na_values = FALSE))

  err <- tryCatch({
    assertthat::assert_that(
      is_integer_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0(
      "Error: v must be a vector of integer values of exact ",
      "length 4 with no NAs. Passed vector length is 3 instead ",
      "of the expected 4\n"
      )
    )

  err <- tryCatch({
    assertthat::assert_that(is_integer_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of integer values of length ",
      "not less than 4 with no NAs. Passed vector length is 3 ",
      "but must be at least 4\n"))

  err <- tryCatch({
    assertthat::assert_that(is_integer_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of integer values of ",
      "length not greater than 2 with no NAs. ",
      "Passed vector length is 3 but must be at most 2\n"))

  err <- tryCatch({
    assertthat::assert_that(is_integer_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of integer values of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Passed vector length is 3 but must be at least 4\n"))

  v <- c(1, NA, 0)

  expect_false(is_integer_vector(v))
  expect_true(is_integer_vector(v, allow_na_values = TRUE))

})

test_that("variableVectorBinary", {
  v1 <- c(0, 1, 1, 1, 0)
  v2 <- c(0, 1, 1, 2, 0)

  expect_true(is_binary_vector(v1))
  expect_false(is_binary_vector(v2))
  expect_error(
    assertthat::assert_that(is_binary_vector(v2)),
    paste0(
      "v2 must be a vector of binary values \\(0 or 1\\) ",
      "possibly uniform with no NAs. Vector contains",
      " elements that are not in the allowed values"
     ))

  v1 <- c(0, 1, NA, 1, 0)

  expect_false(is_binary_vector(v1))
  expect_true(is_binary_vector(v1, allow_na_values = TRUE))

  v1 <- c(1, 1, 1, 1, 1, NA)
  v2 <- c(0, 0, 0, 0)

  expect_true(is_binary_vector(v1, allow_na_values = TRUE))
  expect_false(is_binary_vector(
    v1, allow_na_values = TRUE, allow_uniform = FALSE))

  expect_true(is_binary_vector(v2))
  expect_false(is_binary_vector(v2, allow_uniform = FALSE))

  expect_error(
    assertthat::assert_that(is_binary_vector(v1, allow_uniform = FALSE)),
    paste0(
      "v1 must be a vector of binary values \\(0 or 1\\)",
      " non-uniform with no NAs. Vector contains ",
      "elements that are not in the allowed values"
     ))

  expect_error(
    assertthat::assert_that(is_binary_vector(
      v1, allow_na_values = TRUE, allow_uniform = FALSE
    )),
    paste0(
      "v1 must be a vector of binary values \\(0 or 1\\) non-uniform or NAs. ",
      "Passed vector is uniform on the value 1")
  )

})
