test_that("is_string_value", {
  expect_true(is_string_value("ada"))
  expect_true(is_string_value(""))
  expect_false(is_string_value("", allow_empty = FALSE))
  expect_true(is_string_value(NA_character_, allow_na = TRUE))
  expect_false(is_string_value(2))
  expect_false(is_string_value(NA_character_, allow_na = FALSE))
  expect_false(is_string_value(NA_character_))
  # check for non-string NA (logical in this case.
  # Should return false as it's not a string)
  expect_false(is_string_value(NA, allow_na = TRUE))
  expect_false(is_string_value(c("foo", "bar")))

  err <- tryCatch({
    assertthat::assert_that(is_string_value(4))
    NULL
  },
  error = function(e) {
    return(e)
  })
})

test_that("is_string_vector", {
  v <- c("foo", "bar", "baz")
  expect_true(is_string_vector(v))
  expect_false(is_string_vector(NULL))
  expect_true(is_string_vector(NULL, allow_null = TRUE))
  expect_false(is_string_vector(v, exact_length = 4))
  expect_false(is_vector(v, min_length = 4))
  expect_true(is_vector(v, max_length = 4))
  expect_false(is_vector(v, max_length = 2))
  expect_true(is_vector(v, exact_length = 3, max_length = 2))

  v2 <- "hello"
  expect_true(is_string_vector(v2))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0("Error: v must be a string vector of exact length 4 with no NAs. ",
    "Got: c(\"foo\", \"bar\", \"baz\")\n"))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a string vector of length not less than 4",
      " with no NAs.",
      " Got: c(\"foo\", \"bar\", \"baz\")\n"))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
    "Error: v must be a string vector of length not greater than 2",
    " with no NAs.",
    " Got: c(\"foo\", \"bar\", \"baz\")\n"))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a string vector of length between 4 and 8 inclusive",
      " with no NAs.",
      " Got: c(\"foo\", \"bar\", \"baz\")\n"))
})
