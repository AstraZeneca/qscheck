test_that("is_r6_class", {
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

test_that("is_r6_instance", {
  cls <- R6::R6Class("mycls", list())
  instance <- cls$new()
  expect_true(is_r6_instance(instance, "mycls"))
  expect_false(is_r6_instance(instance, "whatever"))
  expect_false(is_r6_instance(NULL, "mycls"))
  expect_false(is_r6_instance(instance, NULL))
  expect_true(is_r6_instance(NULL, "mycls", allow_null = TRUE))

  err <- tryCatch({
    assertthat::assert_that(is_r6_instance(NULL, "mycls"))
    NULL
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: NULL must be an instance of R6 class mycls. Got: NULL\n"
    )

  err <- tryCatch({
    assertthat::assert_that(is_r6_instance(list(), "mycls", allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: list() must be an instance of R6 class mycls or NULL. ",
      "Got: list()\n")
    )

})
