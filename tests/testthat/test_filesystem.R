test_that("variableIsAFile", {
  f1 <- root_path("tests", "testthat", "test_filesystem.R")
  f2 <- root_path("tests", "testthat", "unexistent.R")
  f3 <- NULL
  d1 <- root_path("tests", "testthat")

  expect_true(is_existing_file(f1))
  expect_false(is_existing_file(f2))
  expect_true(is_existing_file(f3, allow_null = TRUE))
  expect_false(is_existing_file(d1))

  expect_error(
    assertthat::assert_that(is_existing_file(f2)),
    paste(
      "f2 must be a path to an existing file. Path '.*' does not refer to an existing file"
    )
  )

  expect_error(
    assertthat::assert_that(is_existing_file(d1)),
    paste(
      "d1 must be a path to an existing file. Path '.*' is a directory"
    )
  )

})

test_that("variableIsADirectory", {
  d1 <- root_path("tests", "testthat")
  d2 <- root_path("tests", "notexistent")
  d3 <- NULL
  f1 <- root_path("tests", "testthat", "test_filesystem.R")

  expect_true(is_existing_dir(d1))
  expect_false(is_existing_dir(d2))
  expect_true(is_existing_dir(d3, allow_null = TRUE))

  expect_error(
    assertthat::assert_that(is_existing_dir(d2)),
    paste0(
      "d2 must be a path to an existing directory. ",
      "Path '.*' does not refer to an existing directory"
    )
  )

  expect_error(
    assertthat::assert_that(is_existing_dir(f1)),
    paste0(
      "f1 must be a path to an existing directory. ",
      "Path '.*' refers to an existing file instead"
    )
  )

})
