test_that("variableIsAFunction", {
  foo <- function(x, y, z) {
  }

  expect_true(is_function(foo))
  expect_true(is_function(foo, allow_null = TRUE))
  expect_true(is_function(NULL, allow_null = TRUE))
  expect_true(is_function(foo, args = c("x", "y", "z")))
  expect_true(is_function(foo, num_args = 3))

  expect_false(is_function(foo, args = c("x", "z", "y")))
  expect_false(is_function(foo, args = c("x", "y")))
  expect_false(is_function(""))
  expect_false(is_function(foo, num_args = 2))

  expect_false(is_function(tibble::tibble()))

  expect_error(
    assertthat::assert_that(is_function(foo, args = c("x", "z", "y"))),
    paste0(
      "foo must be a function with arguments named 'x', 'z', 'y'. ",
      "Argument 2 was 'y' but 'z' is required"
    )
  )

  expect_error(
    assertthat::assert_that(is_function(foo, num_args = 2)),
    paste0(
      "foo must be a function with 2 arguments. ",
      "Passed function has an incorrect number of arguments \\(3\\)"
    )
  )
})
