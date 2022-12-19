test_that("is_S4_instance", {
  env <- new.env()
  setClass(
    "student", slots = list(name = "character", age = "numeric"),
    where = env)
  s <- new("student", name = "John", age = 21)
  setClass("worker", slots = list(name = "character", age = "numeric"),
    where = env)
  w <- new("worker", name = "Jane", age = 23)
  expect_true(is_s4_instance(s, "student"))
  expect_false(is_s4_instance(w, "student"))
  expect_true(is_s4_instance(w, "worker"))
  expect_false(is_s4_instance(s, "worker"))
  expect_true(is_s4_instance(NULL, "student", allow_null = TRUE))

  expect_error(
    assertthat::assert_that(is_s4_instance(NULL, "student")),
    paste0(
      "NULL must be an instance of S4 class student. ",
      "Passed value cannot be NULL"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_s4_instance(list(), "student", allow_null = TRUE)
    ),
    paste0(
      "list\\(\\) must be an instance of S4 class student or NULL. ",
      "Passed value is not an S4 instance"
    )
  )
})
