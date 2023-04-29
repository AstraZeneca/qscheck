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

})

test_that("variableIsADirectory", {

  f1 <- "./../testthat/"
  f2 <- "./../testthatnot/"
  f3 <- NULL

  expect_true(is_path(f1))
  expect_false(is_path(f2))
  expect_true(is_path(f3, allow_null = TRUE))

  expect_error(
    assertthat::assert_that(is_path(f2)),
    paste(
      "f2 must be a valid directory. Passed value is not a valid directory"
    )
  )

})
