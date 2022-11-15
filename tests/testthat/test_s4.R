test_that("is_S4_instance", {
setClass("student", slots = list(name = "character", age = "numeric"))
s <- new("student", name = "John", age = 21)
setClass("worker", slots = list(name = "character", age = "numeric"))
w <- new("worker", name = "Jane", age = 23)
  expect_true(is_s4_instance(s, "student"))
  expect_false(is_s4_instance(w, "student"))
  expect_true(is_s4_instance(w, "worker"))
  expect_false(is_s4_instance(s, "worker"))
  expect_true(is_s4_instance(NULL, "student", allow_null = TRUE))

  err <- tryCatch({
      assertthat::assert_that(is_s4_instance(NULL, "student"))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: NULL must be an instance of S4 class student. Got: NULL\n")

  err <- tryCatch({
      assertthat::assert_that(is_s4_instance(
        list(), "student", allow_null = TRUE))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )

  expect_equal(
    as.character(err),
    paste0(
      "Error: list() must be an instance of S4 class student or NULL. ",
      "Got: list()\n"))
})
