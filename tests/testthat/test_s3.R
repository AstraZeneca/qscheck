test_that("is_S3_instance", {
  s <- list(name = "John", age = 21)
  class(s) <- c("InternationalStudent", "Student")
  expect_true(is_s3_instance(s, "InternationalStudent"))
  expect_true(is_s3_instance(s, "Student"))
  expect_false(is_s3_instance(s, "list"))
  expect_false(is_s3_instance(NULL, "Student"))
  expect_true(is_s3_instance(NULL, "Student", allow_null = TRUE))

  expect_error(
    assertthat::assert_that(is_s3_instance(NULL, "Student")),
    paste0(
      "NULL must be an instance of S3 class Student. ",
      "Passed value cannot be NULL"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_s3_instance(list(), "Student", allow_null = TRUE)
    ),
    paste0(
      "list\\(\\) must be an instance of S3 class Student or NULL. ",
      "Passed value is not an instance of the specifieed S3 class"
    )
  )
})
