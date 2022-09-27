test_that("is_value", {
  expect_true(is_value(34))
  expect_false(is_value(NA))
  expect_false(is_value(NULL))
  expect_true(is_value(NA, allow_na = TRUE))
  expect_true(is_value(NULL, allow_null = TRUE))
  expect_false(is_value(c(34, 34)))

})

test_that("is_na_value", {
  expect_true(is_na_value(NA))
  expect_false(is_na_value(c(NA, NA)))
  expect_false(is_na_value("hello"))

  err <- tryCatch({
    foo <- 3
    assertthat::assert_that(is_na_value(foo))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    "Error: foo must be NA. Got: 3\n")
})
