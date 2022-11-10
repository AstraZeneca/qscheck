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
  err <- tryCatch({
      assertthat::assert_that(is_real_value(foo))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: foo must be a single real value. Got: \"hello\"\n")
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
  err <- tryCatch({
      assertthat::assert_that(is_probability_value(foo))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: foo must be a single probability value in the interval [0.0, 1.0]. Got: -0.3\n")
})

test_that("is_positive_real_value", {
  expect_true(is_positive_real_value(1))
  expect_true(is_positive_real_value(1.01))
  expect_true(is_positive_real_value(NULL, allow_null = TRUE))
  expect_true(is_positive_real_value(NA_real_, allow_na = TRUE))
  expect_false(is_positive_real_value("1"))
  expect_false(is_positive_real_value(c(1.2, 1.3)))

  foo <- -1.5
  err <- tryCatch({
      assertthat::assert_that(is_positive_real_value(foo))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a positive real value. Got: -1.5\n")

  foo <- -1.5
  err <- tryCatch({
      assertthat::assert_that(is_positive_real_value(
        foo, allow_na = TRUE, allow_null = TRUE))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a positive real value or NA or NULL. Got: -1.5\n")
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
  err <- tryCatch({
      assertthat::assert_that(is_non_negative_real_value(foo))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a non-negative real value. Got: -1.5\n")

  foo <- -1.5
  err <- tryCatch({
      assertthat::assert_that(is_non_negative_real_value(
        foo, allow_na = TRUE, allow_null = TRUE))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a non-negative real value or NA or NULL. Got: -1.5\n")
})

test_that("is_binary_vector", {
  v1 <- c(0, 1, 1, 1, 0)
  v2 <- c(0, 1, 1, 2, 0)

  expect_true(is_binary_vector(v1))
  expect_false(is_binary_vector(v2))

  err <- tryCatch({
      assertthat::assert_that(is_binary_vector(v2))
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: v2 must be a vector of binary values (0 or 1)\n"
     ))
})

test_that("is_binary_vector with NA", {
  v1 <- c(0, 1, NA, 1, 0)

  expect_false(is_binary_vector(v1))
  expect_true(is_binary_vector(v1, allow_na_values = TRUE))

})

test_that("is_binary_vector allow_degenerate", {
  v1 <- c(1, 1, 1, 1, 1, NA)
  v2 <- c(0, 0, 0, 0)

  expect_true(is_binary_vector(v1, allow_na_values = TRUE))
  expect_false(is_binary_vector(
    v1, allow_na_values = TRUE, allow_degenerate = FALSE))

  expect_true(is_binary_vector(v2))
  expect_false(is_binary_vector(v2, allow_degenerate = FALSE))

  err <- tryCatch({
      assertthat::assert_that(is_binary_vector(v1, allow_degenerate = FALSE))
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: v1 must be a non-degenerate vector of binary values (0 or 1)\n"
     ))
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

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0("Error: v must be a vector of real numbers of exact length 4",
    " with no NAs. ",
    "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of real numbers of length not less than 4",
      " with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of real numbers of length not ",
      "greater than 2 with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))
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

  err <- tryCatch({
    assertthat::assert_that(is_positive_real_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0("Error: v must be a vector of positive real numbers of exact length 4",
    " with no NAs. ",
    "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_positive_real_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of positive real numbers of length not less than 4",
      " with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_positive_real_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of positive real numbers of length not ",
      "greater than 2 with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_positive_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of positive real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))
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

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_real_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0("Error: v must be a vector of non-negative real numbers of exact length 4",
    " with no NAs. ",
    "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_real_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non-negative real numbers of length not less than 4",
      " with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_real_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non-negative real numbers of length not ",
      "greater than 2 with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_real_vector(v,
      min_length = 4, max_length = 8, allow_na_values = FALSE))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non-negative real numbers of length ",
      "between 4 and 8 inclusive with no NAs. ",
      "Got: c(1.2, 1.3, 3)\n"))
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

  expect_error({
    v <- c(1.2, 1.2, 3.0)
    assertthat::assert_that(is_increasing_vector(v, strictly = TRUE))
  }, regexp = "v must be a vector of strictly increasing numbers with no NAs."
  )

  expect_error({
    v <- c(3.2, 1.2, 3.0)
    assertthat::assert_that(is_increasing_vector(v))
  }, regexp = "v must be a vector of increasing numbers with no NAs."
  )

  expect_error({
    v <- c(3.2, 1.2, 3.0)
    assertthat::assert_that(is_increasing_vector(v, allow_na_values = TRUE))
  }, regexp = "v must be a vector of increasing numbers or NAs."
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

  expect_error({
    v <- c(3.0, 1.2, 1.2)
    assertthat::assert_that(is_decreasing_vector(v, strictly = TRUE))
  }, regexp = "v must be a vector of strictly decreasing numbers with no NAs."
  )

  expect_error({
    v <- c(3.2, 1.2, 3.0)
    assertthat::assert_that(is_decreasing_vector(v))
  }, regexp = "v must be a vector of decreasing numbers with no NAs."
  )

  expect_error({
    v <- c(3.2, 1.2, 3.0)
    assertthat::assert_that(is_decreasing_vector(v, allow_na_values = TRUE))
  }, regexp = "v must be a vector of decreasing numbers or NAs."
  )

})
