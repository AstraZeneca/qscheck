
test_that("is_factor", {
  expect_false(is_factor(data.frame()))
  expect_false(is_factor(NULL))
  expect_true(is_factor(NULL, allow_null = TRUE))
  expect_true(is_factor(factor(c("foo", "bar"))))

  expect_false(is_factor(factor(c("foo", "bar")), exact_levels = c("foo", "yup")))
  expect_true(is_factor(factor(c("foo", "bar", "bar")), exact_levels = c("bar", "foo")))
  expect_true(is_factor(factor(c("foo", "bar", "bar")), exact_length = 3))
  expect_false(is_factor(factor(c("foo", "bar")), exact_length = 3))
  expect_false(is_factor(factor(c("foo", "bar", NA_character_)), exact_length = 3))

  err <- tryCatch({
    assertthat::assert_that(is_factor(3, exact_levels = c("bar", "foo"), allow_null = TRUE))
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
      "with no NAs; or NULL. passed value is not a factor\n"
    )
  )
})
