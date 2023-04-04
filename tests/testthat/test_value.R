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

test_that("lessThanCorrectValue", {
  expect_true(
    assertthat::assert_that(is_lt_value(4, 6))
  )
})

test_that("lessThanNULLAllowed", {
  expect_true(
    assertthat::assert_that(is_lt_value(NULL, 6, allow_null = TRUE))
  )
})

test_that("lessThanNumericalNA", {
  expect_true(
    assertthat::assert_that(is_lt_value(value = NA_real_ , comparator = 6, allow_na = TRUE))
  )
})

test_that("lessThanAssertFails", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 6, comparator = 5)),
    "6 must be a smaller value than 5. Passed value 6 is greater than or equal to the maximum of 5"
  )
})

test_that("lessThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = "hello", comparator = 6)),
    "hello must be a smaller value than 6. Passed value is not a numerical"
  )
})

test_that("lessThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 5, comparator = "hello")),
    "5 must be a smaller value than hello. Passed comparator value is not a numerical"
  )
})