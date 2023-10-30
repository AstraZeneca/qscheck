test_that("variableLogicalValue", {
  expect_true(is_logical_value(FALSE))
  expect_false(is_logical_value(c(FALSE, FALSE)))
  expect_false(is_logical_value(""))
  expect_false(is_logical_value(NA_real_))
  expect_false(is_logical_value(NA))
  expect_false(is_logical_value(NULL))

  expect_true(is_logical_value(NA, allow_na = TRUE))
  expect_true(is_logical_value(NULL, allow_null = TRUE))

  foo <- 1.5
  expect_error(
    assertthat::assert_that(is_logical_value(foo)),
    "foo must be a logical value. Passed value is not a logical"
  )
  expect_error(
    assertthat::assert_that(is_logical_value(foo, allow_na = TRUE)),
    "foo must be a logical value or NA. Passed value is not a logical"
  )
  expect_error(
    assertthat::assert_that(is_logical_value(foo, allow_null = TRUE)),
    "foo must be a logical value or NULL. Passed value is not a logical"
  )
})
