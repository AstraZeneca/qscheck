test_that("is_logical_value", {
  expect_true(is_logical_value(FALSE))
  expect_false(is_logical_value(c(FALSE, FALSE)))
  expect_false(is_logical_value(""))
  foo <- -1.5
  err <- tryCatch({
      assertthat::assert_that(is_logical_value(foo))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a single logical value. Got: -1.5\n")
})
