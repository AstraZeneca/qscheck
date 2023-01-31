test_that("presenceOfVariableIsDependentOnAnother", {
  expect_true(consequential(NULL, NULL))
  expect_false(consequential(1, NULL))
  expect_false(consequential(NULL, 1))
  expect_true(consequential(1, "hello"))

  foo <- -0.3
  bar <- NULL
  expect_error(
    assertthat::assert_that(consequential(foo, bar)),
    "If foo is not NULL, bar must also be not NULL. The second argument is NULL"
  )
  expect_error(
    assertthat::assert_that(consequential(bar, foo)),
    "If bar is NULL, foo must also be NULL. The second argument is not NULL"
  )
})
