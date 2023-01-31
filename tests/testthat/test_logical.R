test_that("variableLogicalValue", {
  expect_true(is_logical_value(FALSE))
  expect_false(is_logical_value(c(FALSE, FALSE)))
  expect_false(is_logical_value(""))

  foo <- 1.5
  expect_error(
    assertthat::assert_that(is_logical_value(foo)),
    "foo must be a logical value. Passed value is not a logical")
})
