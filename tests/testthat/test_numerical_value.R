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
    "6 must be a numerical value smaller than 5. Passed value 6 is greater than or equal to 5"
  )
})

test_that("lessThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = "hello", comparator = 6)),
    "hello must be a numerical value smaller than 6. Passed value is not a numerical"
  )
})

test_that("lessThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 5, comparator = "hello")),
    paste0(
      "Incorrect invocation\\. Parameter 'comparator' is invalid: ",
      "Passed value is not a numerical"
    )
  )
})

test_that("lessThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 5, comparator = NULL)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NULL"
  )
})

test_that("lessThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_lt_value(value = 5, comparator = NA_real_)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NA"
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
    "6 must be a numerical value smaller than or equal to 5. Passed value 6 is greater than 5"
  )
})

test_that("lessOrEqualThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = "hello", comparator = 6)),
    "hello must be a numerical value smaller than or equal to 6. Passed value is not a numerical"
  )
})

test_that("lessOrEqualThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 5, comparator = "hello")),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is not a numerical"
  )
})

test_that("lessOrEqualThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 5, comparator = NULL)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NULL"
  )
})

test_that("lessOrEqualThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_lte_value(value = 5, comparator = NA_real_)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NA"
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
    "5 must be a numerical value greater than 6. Passed value 5 is smaller than or equal to 6"
  )
})

test_that("greaterThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = "hello", comparator = 6)),
    "hello must be a numerical value greater than 6. Passed value is not a numerical"
  )
})

test_that("greaterThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = "hello")),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is not a numerical"
  )
})

test_that("greaterThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = NULL)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NULL"
  )
})

test_that("greaterThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_gt_value(value = 5, comparator = NA_real_)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NA"
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
    "5 must be a numerical value greater than or equal to 6. Passed value 5 is smaller than 6"
  )
})

test_that("greaterOrEqualThanNotNumericalValue", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = "hello", comparator = 6)),
    "hello must be a numerical value greater than or equal to 6. Passed value is not a numerical"
  )
})

test_that("greaterOrEqualThanComparatorNotNumerical", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = "hello")),
    paste0(
      "Incorrect invocation\\. Parameter 'comparator' is invalid: ",
      "Passed value is not a numerical"
    )
  )
})

test_that("greaterOrEqualThanComparatorNA", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = NA_real_)),
    paste0(
      "Incorrect invocation\\. Parameter 'comparator' is invalid: ",
      "Passed value is NA"
    )
  )
})
test_that("greaterOrEqualThanComparatorNULL", {
  expect_error(
    assertthat::assert_that(is_gte_value(value = 5, comparator = NULL)),
    "Incorrect invocation\\. Parameter 'comparator' is invalid: Passed value is NULL"
  )
})
