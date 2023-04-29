test_that("variableIsAFile", {

  f1 <- "./test_files.R"
  f2 <- "./test_filesss.R"
  f3 <- NULL

  expect_true(is_file(f1))
  expect_false(is_file(f2))
  expect_true(is_file(f3, allow_null = TRUE))

  expect_error(
    assertthat::assert_that(is_file(f2)),
    paste(
      "f2 must be a file. Passed value is not a file"
    )
  )

if (FALSE) {




  expect_error(
    assertthat::assert_that(is_function(foo, num_args = 2)),
    paste0(
      "foo must be a function with 2 arguments. ",
      "Passed function has an incorrect number of arguments \\(3\\)"
    )
  )

}

})
