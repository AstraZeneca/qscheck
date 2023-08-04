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

test_that("passedEntityIsNARealValue", {
  expect_true(is_na_real_value(NA_real_))
  expect_false(is_na_real_value(NA_character_))
  expect_false(is_na_real_value(c(NA_real_, NA_real_)))
  expect_false(is_na_real_value("hello"))

  foo <- 3
  expect_error(
    assertthat::assert_that(is_na_real_value(foo)),
    "foo must be a NA_real_. Passed value is not NA"
  )
  foo <- NA_character_
  expect_error(
    assertthat::assert_that(is_na_real_value(foo)),
    "foo must be a NA_real_. Passed value is NA of incorrect type character"
  )
})

test_that("passedEntityIsNACharacterValue", {
  expect_true(is_na_character_value(NA_character_))
  expect_false(is_na_character_value(NA_real_))
  expect_false(is_na_character_value(c(NA_character_, NA_character_)))
  expect_false(is_na_character_value("hello"))

  foo <- 3
  expect_error(
    assertthat::assert_that(is_na_character_value(foo)),
    "foo must be a NA_character_. Passed value is not NA"
  )
  foo <- NA_real_
  expect_error(
    assertthat::assert_that(is_na_character_value(foo)),
    paste(
      "foo must be a NA_character_.",
      "Passed value is NA of incorrect type numeric"
    )
  )
})


test_that("passedEntityIsNALogicalValue", {
  expect_true(is_na_logical_value(NA))
  expect_false(is_na_logical_value(NA_real_))
  expect_false(is_na_logical_value(c(NA, NA)))
  expect_false(is_na_logical_value("hello"))

  foo <- 3
  expect_error(
    assertthat::assert_that(is_na_logical_value(foo)),
    "foo must be a logical NA. Passed value is not NA"
  )
  foo <- NA_real_
  expect_error(
    assertthat::assert_that(is_na_logical_value(foo)),
    paste(
      "foo must be a logical NA.",
      "Passed value is NA of incorrect type numeric"
    )
  )
})
