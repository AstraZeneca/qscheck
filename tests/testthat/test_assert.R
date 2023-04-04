test_that("assertionViaQScheck", {
  err <- tryCatch({
    assert(is_factor(3, exact_levels = c("bar", "foo"), allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: 3 must be a factor with exact levels ('bar', 'foo') ",
      "with no NAs; or NULL. Passed value is not a factor\n"
    )
  )
})

test_that("messageReturnsStringOnFailingAssert", {
  expect_equal(
    message(is_factor(3, exact_levels = c("bar", "foo"), allow_null = TRUE)),
    paste0(
      "3 must be a factor with exact levels ('bar', 'foo') ",
      "with no NAs; or NULL. Passed value is not a factor"
    )
  )
})

test_that("messageReturnsNULLOnSuccessfulAssert", {
  expect_true(is.null(message(is_integer_value(3))))
})

test_that("validateMessageOnFailure", {
  expect_equal(
    validate(is_string_value(3)),
    "3 must be a string. Passed value is not a character"
  )
})

test_that("validateMessageOnSuccess", {
  expect_true(validate(is_string_value("hello")))
})
