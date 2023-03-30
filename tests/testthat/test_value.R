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

test_that("greater_than_value", {
  expect_true(is_greater_than_value(6, 5))
  expect_true(is_greater_than_value(6, -5))
  expect_true(is_greater_than_value(6, 6, strictly = FALSE))
  expect_false(is_greater_than_value(6, 6, strictly = TRUE))
  expect_false(is_greater_than_value(5, 6))
  expect_false(is_greater_than_value(-6, 5))
  expect_error(
    assertthat::assert_that(is_greater_than_value("hello", 5, allow_na = TRUE)),
    " 'hello' must be a greater value than 5 or NA. Passed value is not a numerical"
  )
  expect_error(
    assertthat::assert_that(is_greater_than_value(value = NULL, 5)),
    "Passed value is NULL"
  )
  expect_error(
    assertthat::assert_that(is_greater_than_value(5, NULL, allow_null = TRUE)),
    "Passed comparator value is NULL"
  )
})

test_that("less_than_value", {
  expect_true(is_less_than_value(5, 6))
  expect_true(is_less_than_value(-5, 6))
  expect_true(is_less_than_value(6, 6, strictly = FALSE))
  expect_false(is_less_than_value(6, 6, strictly = TRUE))
  expect_false(is_less_than_value(6, 5))
  expect_false(is_less_than_value(5, -6))
  expect_error(
    assertthat::assert_that(is_less_than_value("hello", 5, allow_na = TRUE)),
    " \'hello\' must be a smaller value than 5 or NA. Passed value is not a numerical"
  )
  expect_error(
    assertthat::assert_that(is_less_than_value(value = NULL, 5)),
    "Passed value is NULL"
  )
  expect_error(
    assertthat::assert_that(is_less_than_value(5, NULL, allow_null = TRUE)),
    "Passed comparator value is NULL"
  )
})