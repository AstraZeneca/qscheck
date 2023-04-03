test_that("entityIsASingleValue", {
  expect_true(is_value(34))
  expect_false(is_value(NA))
  expect_false(is_value(NULL))
  expect_true(is_value(NA, allow_na = TRUE))
  expect_true(is_value(NULL, allow_null = TRUE))
  expect_false(is_value(c(34, 34)))

})

test_that("passedEntityIsNAValue", {
  expect_true(is_na_value(NA))
  expect_false(is_na_value(c(NA, NA)))
  expect_false(is_na_value("hello"))

  foo <- 3
  expect_error(
    assertthat::assert_that(is_na_value(foo)),
    "foo must be a NA \\(any type\\). Passed value is not NA"
  )
})
