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
    "5 must be a smaller value than hello\\. Invalid comparator value: Passed value is not a numerical"
  )
})

test_that("lessThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 5, comparator = NULL)),
    "Passed comparator is NULL"
  )
})

test_that("lessThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 5, comparator = NA_real_)),
    "5 must be a smaller value than NA. Invalid comparator value: Passed value is NA"
  )
})

test_that("lessOrEqualThanCorrectValue", {
  expect_true(
    assertthat::assert_that(is_lte_value(4, 4))
  )
})

test_that("lessOrEqualThanNULLAllowed", {
  expect_true(
    assertthat::assert_that(is_lte_value(NULL, 6, allow_null = TRUE))
  )
})

test_that("lessOrEqualThanNumericalNA", {
  expect_true(
    assertthat::assert_that(is_lte_value(value = NA_real_ , comparator = 6, allow_na = TRUE))
  )
})

test_that("lessOrEqualThanAssertFails", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 6, comparator = 5)),
    "6 must be a value smaller than or equal to 5. Passed value 6 is greater than the maximum of 5"
  )
})

test_that("lessOrEqualThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = "hello", comparator = 6)),
    "hello must be a value smaller than or equal to 6. Passed value is not a numerical"
  )
})

test_that("lessOrEqualThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 5, comparator = "hello")),
    "5 must be a value smaller than or equal to hello. Invalid comparator value: Passed value is not a numerical"
  )
})

test_that("lessOrEqualThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 5, comparator = NULL)),
    "Passed comparator is NULL"
  )
})

test_that("lessOrEqualThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 5, comparator = NA_real_)),
    "5 must be a value smaller than or equal to NA. Invalid comparator value: Passed value is NA"
  )
})

test_that("greaterThanCorrectValue", {
  expect_true(
    assertthat::assert_that(is_gt_value(6, 4))
  )
})

test_that("greaterThanNULLAllowed", {
  expect_true(
    assertthat::assert_that(is_gt_value(NULL, 6, allow_null = TRUE))
  )
})

test_that("greaterThanNumericalNA", {
  expect_true(
    assertthat::assert_that(is_gt_value(value = NA_real_ , comparator = 6, allow_na = TRUE))
  )
})

test_that("greaterThanAssertFails", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = 6)),
    "5 must be a greater value than 6. Passed value 5 is less than or equal to the minimum of 6"
  )
})

test_that("greaterThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = "hello", comparator = 6)),
    "hello must be a greater value than 6. Passed value is not a numerical"
  )
})

test_that("greaterThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = "hello")),
    "5 must be a greater value than hello. Invalid comparator value: Passed value is not a numerical"
  )
})

test_that("greaterThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = NULL)),
    "Passed comparator is NULL"
  )
})

test_that("greaterThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = NA_real_)),
    "5 must be a greater value than NA. Invalid comparator value: Passed value is NA"
  )
})

test_that("greaterOrEqualThanCorrectValue", {
  expect_true(
    assertthat::assert_that(is_gte_value(4, 4))
  )
})

test_that("greaterOrEqualThanNULLAllowed", {
  expect_true(
    assertthat::assert_that(is_gte_value(NULL, 6, allow_null = TRUE))
  )
})

test_that("greaterOrEqualThanNumericalNA", {
  expect_true(
    assertthat::assert_that(is_gte_value(value = NA_real_ , comparator = 6, allow_na = TRUE))
  )
})

test_that("greaterOrEqualThanAssertFails", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = 6)),
    "5 must be a value greater than or equal to 6. Passed value 5 is lower than the minimum of 6"
  )
})

test_that("greaterOrEqualThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = "hello", comparator = 6)),
    "hello must be a value greater than or equal to 6. Passed value is not a numerical"
  )
})

test_that("greaterOrEqualThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = "hello")),
    "5 must be a value greater than or equal to hello. Invalid comparator value: Passed value is not a numerical"
  )
})

test_that("greaterOrEqualThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = NULL)),
    "Passed comparator is NULL"
  )
})

test_that("greaterOrEqualThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = NA_real_)),
    "5 must be a value greater than or equal to NA. Invalid comparator value: Passed value is NA"
  )
})
