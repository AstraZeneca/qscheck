test_that("is_integer_value", {
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
    "Error: foo must be an integer value in the range [3, inf). Got: 2\n")

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, max = 1))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be an integer value in the range (-inf, 1]. Got: 2\n")

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, min = 3, max = 5))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be an integer value in the range [3, 5]. Got: 2\n")

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
      " Got: structure(list(), .Names = character(0), row.names = ",
      "integer(0), class = \"data.frame\")\n"))

})

test_that("is_positive_integer_value", {
  expect_true(is_positive_integer_value(3))
  expect_true(is_positive_integer_value(3.0))
  expect_false(is_positive_integer_value(3.5))
  expect_false(is_positive_integer_value(0))
  expect_false(is_positive_integer_value(c(3, 4)))
  expect_false(is_positive_integer_value(-3))
  expect_false(is_positive_integer_value("hello"))
  expect_false(is_positive_integer_value(NULL))
  expect_true(is_positive_integer_value(NULL, allow_null = TRUE))
})

test_that("is_non_negative_integer_value", {
  expect_true(is_non_negative_integer_value(3))
  expect_true(is_non_negative_integer_value(3.0))
  expect_true(is_non_negative_integer_value(0))
  expect_false(is_non_negative_integer_value(3.5))
  expect_false(is_non_negative_integer_value(c(3, 4)))
  expect_false(is_non_negative_integer_value(-3))
  expect_false(is_non_negative_integer_value("hello"))
  expect_false(is_non_negative_integer_value(NULL))
  expect_true(is_non_negative_integer_value(NULL, allow_null = TRUE))
})


test_that("is_non_negative_integer_vector", {
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
      "length 4 with no NAs. Got: c(1, 13, 0)\n"))

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
      "not less than 4 with no NAs. Got: c(1, 13, 0)\n"))

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
      "length not greater than 2 with no NAs. Got: c(1, 13, 0)\n"))

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
      "Got: c(1, 13, 0)\n"))
})

test_that("is_non_negative_integer_vector with NA", {
  v <- c(1, NA, 0)

  expect_false(is_non_negative_integer_vector(v))
  expect_true(is_non_negative_integer_vector(v, allow_na_values = TRUE))

})

test_that("is_positive_integer_vector", {
  v <- c(1, 13, 1)
  v1 <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)

  expect_true(is_positive_integer_vector(v))
  expect_false(is_positive_integer_vector(v1))
  expect_false(is_positive_integer_vector(NULL))
  expect_false(is_positive_integer_vector(v2))
  expect_false(is_positive_integer_vector(v3))

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
      "length 4 with no NAs. Got: c(1, 13, 1)\n"))

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
      "not less than 4 with no NAs. Got: c(1, 13, 1)\n"))

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
      "length not greater than 2 with no NAs. Got: c(1, 13, 1)\n"))

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
      "Got: c(1, 13, 1)\n"))
})

test_that("is_positive_integer_vector with NA", {
  v <- c(1, NA, 20)

  expect_false(is_positive_integer_vector(v))
  expect_true(is_positive_integer_vector(v, allow_na_values = TRUE))

})

test_that("is_integer_vector", {
  v <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)

  expect_true(is_integer_vector(v))
  expect_false(is_integer_vector(NULL))
  expect_false(is_integer_vector(v2))
  expect_true(is_integer_vector(v3))

  expect_false(is_integer_vector(v, exact_length = 4))
  expect_false(is_integer_vector(v, min_length = 4))
  expect_true(is_integer_vector(v, max_length = 4))
  expect_false(is_integer_vector(v, max_length = 2))
  expect_true(is_integer_vector(
    v, exact_length = 3, max_length = 2))

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
      "length 4 with no NAs. Got: c(1, 13, 0)\n"))

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
      "not less than 4 with no NAs. Got: c(1, 13, 0)\n"))

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
      "length not greater than 2 with no NAs. Got: c(1, 13, 0)\n"))

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
      "Got: c(1, 13, 0)\n"))
})

test_that("is_integer_vector with NA", {
  v <- c(1, NA, 0)

  expect_false(is_integer_vector(v))
  expect_true(is_integer_vector(v, allow_na_values = TRUE))

})
