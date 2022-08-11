test_that("any_satisfied", {
  expect_true(any_satisfied(is_string_value(""), is_logical_value("")))
  expect_true(any_satisfied(is_string_value(FALSE), is_logical_value(FALSE)))
  expect_false(
    any_satisfied(
      is_string_value(list()),
      is_logical_value(list())
    )
  )
  err <- tryCatch({
    assertthat::assert_that(
      any_satisfied(
        is_string_value(list()),
        is_logical_value(list())
      )
    )
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste(
      "Error: any_satisfied(is_string_value(list()),",
      "is_logical_value(list())): none of the possible",
      "options could be satisfied.\n"
    )
  )

})

test_that("mutually_exclusive", {
  expect_true(mutually_exclusive(NULL, NULL, NULL))
  expect_false(mutually_exclusive(NULL, NULL, NULL, allow_all_null = FALSE))
  expect_true(mutually_exclusive(1, NULL, NULL))
  expect_true(mutually_exclusive(TRUE, NULL, NULL))
  expect_true(mutually_exclusive(NULL, TRUE, NULL))
  expect_true(mutually_exclusive(NULL, NULL, 3))
  expect_false(mutually_exclusive(TRUE, NULL, 24))
  expect_false(mutually_exclusive(TRUE, "hello", 24))

  foo <- -0.3
  bar <- NULL
  baz <- 5
  err <- tryCatch({
      assertthat::assert_that(mutually_exclusive(foo, bar, baz))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste(
      "Error: 'foo', 'baz' must be mutually exclusive",
      "or all NULL. Got -0.3, 5\n"
    )
  )
})

test_that("#40: mutually_exclusive ", {
  foo <- function(a = NULL, b = NULL) {
    assertthat::assert_that(mutually_exclusive(a, b, allow_all_null = FALSE))
  }

  a <- NULL
  b <- NULL
  expect_error({
    assertthat::assert_that(mutually_exclusive(
      a, b, allow_all_null = FALSE)
    )
  }, regexp = paste(
    "'a', 'b' must be mutually exclusive with exactly",
    "one non-NULL element. Got all NULLs")
  )
})

test_that("is_one_of", {
  options <- c("foo", "bar", "baz")

  expect_true(is_one_of("foo", options))
  expect_false(is_one_of("fux", options))
  expect_false(is_one_of("1", c(1, 2, 3)))
  expect_false(is_one_of(c(1, 2), c(1, 2, 3)))

  val <- "x"
  err <- tryCatch({
      assertthat::assert_that(is_one_of(
        val,
        c("foo", "quux", NA_character_),
        allow_null = TRUE))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: val must be one of the following: 'foo', 'quux', 'NA' or NULL. ",
      "Got: \"x\"\n"
     ))
})
