test_that("is_vector", {
  v <- c(1, 2, 3)
  expect_true(is_vector(v))
  expect_false(is_vector(NULL))
  expect_false(is_vector(v, exact_length = 4))
  expect_false(is_vector(v, min_length = 4))
  expect_true(is_vector(v, max_length = 4))
  expect_false(is_vector(v, max_length = 2))
  expect_true(is_vector(v, exact_length = 3, max_length = 2))

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0(
      "Error: v must be a vector of exact length 4. ",
      "Passed vector length is 3 instead of the expected 4\n"
    ))

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of length not less than 4. ",
      "Passed vector length is 3 but must be at least 4\n"
    )
  )

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0("Error: v must be a vector of length not greater than 2. ",
    "Passed vector length is 3 but must be at most 2\n"))

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of length between 4 and 8 inclusive. ",
      "Passed vector length is 3 but must be at least 4\n"))
})

test_that("vectors_disjoint", {
  v1 <- c("foo", "bar", "baz")
  v2 <- c(2, 3, 4)
  v3 <- c(2, "foo", 4)
  v4 <- c(5, 6, 7)
  v5 <- c(7, 8, 5)
  expect_true(vectors_disjoint(v1, v2))
  expect_false(vectors_disjoint(v1, v3))
  expect_true(vectors_disjoint(v2, v4))
  expect_true(vectors_disjoint(v2, v3))
  expect_false(vectors_disjoint(v4, v5))

  err <- tryCatch({
    assertthat::assert_that(vectors_disjoint(v1, v3))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v1 and v3 must be disjoint vectors but the following elements ",
      "are present in both: foo\n"
      ))

  err <- tryCatch({
    assertthat::assert_that(vectors_disjoint(v4, v5))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v4 and v5 must be disjoint vectors but the following elements ",
      "are present in both: 5, 7\n"
      ))

  df <- data.frame()
  err <- tryCatch({
    assertthat::assert_that(vectors_disjoint(v1, df))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    "Error: df must be a vector. Got: data.frame\n"
  )
})

test_that("vector_value_occurrences", {
  vec <- c(1, 2, 3)
  val <- 3
  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(vec, val, exact_occurrences = 2)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' exactly 2 times. ",
      "Found it 1 times.\n"
    )
  )

  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(vec, val, min_occurrences = 2)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' no less than ",
      "2 times inclusive. Found it 1 times.\n"
    )
  )

  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(
          vec, val, min_occurrences = 2, max_occurrences = 4)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' between 2 and 4 ",
      "times inclusive. Found it 1 times.\n"
    )
  )

  vec <- c(1, 2, 3, 3, 3, 3)
  val <- 3

  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(
          vec, val, max_occurrences = 3)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' no more than ",
      "3 times inclusive. Found it 4 times.\n"
    )
  )

})

test_that("vector_allowed_values", {
  v1 <- c("foo", "bar", "bar", NA_character_)

  expect_true(vector_allowed_values(v1, c("foo", "bar", "baz", NA_character_)))
  expect_true(vector_allowed_values(v1, c("foo", "bar", NA_character_)))
  expect_false(vector_allowed_values(v1, c("foo")))

  err <- tryCatch({
      assertthat::assert_that(vector_allowed_values(
        v1,
        c("foo", "quux", NA_character_)))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: v1 must be a vector containing only elements from the ",
      "following list: 'foo', 'quux', NA. Vector contains elements that ",
      "are not in the allowed_values\n"
     ))
})
test_that("is_vector_without_na", {
  expect_false(is_vector_without_na(c(1, NA, 2)))
  expect_true(is_vector_without_na(c(1, 2)))

})

test_that("is_vector_all_na", {
  expect_false(is_vector_all_na(c(1, NA, 2)))
  expect_true(is_vector_all_na(c(NA, NA)))

})
