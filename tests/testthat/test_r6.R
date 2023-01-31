test_that("variableIsR6Class", {
  cls <- R6::R6Class("mycls", list())
  expect_true(is_r6_class(cls, "mycls"))
  expect_false(is_r6_class(cls, "whatever"))
  expect_false(is_r6_class(NULL, "mycls"))

  expect_error(
    assertthat::assert_that(is_r6_class(cls, "whatever")),
    "cls must be a R6 class 'whatever'. Passed value is an R6 class 'mycls'"
  )

  expect_error(
    assertthat::assert_that(is_r6_class(NULL, "mycls")),
    "NULL must be a R6 class 'mycls'. Passed class cannot be NULL"
  )

})

test_that("variableIsR6Instance", {
  cls <- R6::R6Class("mycls", list())
  instance <- cls$new()
  lst <- list()
  expect_true(is_r6_instance(instance, "mycls"))
  expect_false(is_r6_instance(instance, "whatever"))
  expect_false(is_r6_instance(NULL, "mycls"))
  expect_true(is_r6_instance(NULL, "mycls", allow_null = TRUE))

  expect_error(
    assertthat::assert_that(is_r6_instance(NULL, "mycls")),
    "NULL must be an instance of R6 class 'mycls'. Passed value is NULL"
  )

  expect_error(
    assertthat::assert_that(is_r6_instance(lst, "mycls", allow_null = TRUE)),
    paste0(
      "lst must be an instance of R6 class 'mycls' or NULL. ",
      "Passed value is not an R6 instance"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_r6_instance(instance, "othercls", allow_null = TRUE)
    ),
    paste0(
      "instance must be an instance of R6 class 'othercls' or NULL. ",
      "Passed value is not an instance of class 'othercls'"
    )
  )

})
