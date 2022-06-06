test_that("is_S3_instance", {
  s <- list(name = "John", age = 21)
  class(s) <- c("InternationalStudent", "Student")
  expect_true(is_s3_instance(s, "InternationalStudent"))
  expect_true(is_s3_instance(s, "Student"))
  expect_false(is_s3_instance(s, "list"))
  expect_false(is_s3_instance(NULL, "Student"))
  expect_true(is_s3_instance(NULL, "Student", allow_null = TRUE))

  err <- tryCatch({
      assertthat::assert_that(is_s3_instance(NULL, "Student"))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: NULL must be an instance of S3 class Student. Got: NULL\n")

  err <- tryCatch({
      assertthat::assert_that(is_s3_instance(
        list(), "Student", allow_null = TRUE))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )

  expect_equal(
    as.character(err),
    paste0(
      "Error: list() must be an instance of S3 class Student or NULL. ",
      "Got: list()\n"))
})
