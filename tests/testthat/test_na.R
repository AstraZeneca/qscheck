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
