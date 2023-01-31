test_that("variableIsAList", {
  # this is expected. data frame is not inheriting from list.
  expect_false(is_list(data.frame()))

  expect_false(is_list(NULL))
  expect_true(is_list(NULL, allow_null = TRUE))
  expect_true(is_list(list()))

  expect_false(is_list(list(foo = "bar"), required_names = c("foo", "yup")))
  expect_true(is_list(list(foo = "bar", yup = "xxx"), required_names = c("foo")))
  expect_true(is_list(list(foo = "bar", yup = "xxx"), exact_length = 2))
  expect_false(is_list(list(foo = "bar", yup = "xxx"), exact_length = 3))
  expect_false(is_list(list(foo = "bar", yup = "xxx"), exact_length = 1))

  err <- tryCatch({
    assertthat::assert_that(is_list(3, allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(as.character(err),
    "Error: 3 must be a list or NULL. The passed entity is not a list\n")
})
