test_that("is_data_frame", {
  df <- data.frame(
      foo = c(1, 2, 3),
      bar = c(3, 4, 5)
  )
  empty <- data.frame()
  expect_true(is_data_frame(df))
  expect_true(is_data_frame(df, allow_na = TRUE))
  expect_true(is_data_frame(NA, allow_na = TRUE))
  expect_false(is_data_frame(NA, allow_na = FALSE))
  expect_true(is_data_frame(df, required_colnames = c("foo")))
  expect_false(is_data_frame(df, required_colnames = c("baz")))
  expect_false(is_data_frame(df, exact_colnames = c("foo", "bar", "baz")))
  expect_false(is_data_frame(df, exact_colnames = c("foo")))
  expect_true(is_data_frame(df, exact_colnames = c("foo", "bar")))

  err <- tryCatch({
      assertthat::assert_that(is_data_frame(df, required_colnames = c("baz")))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0("Error: df must be a data frame with required colnames 'baz'. ",
          "Got: structure(list(foo = c(1, 2, 3), bar = c(3, 4, 5)), ",
           "class = \"data.frame\", row.names = c(NA, -3L))\n")
    )

  err <- tryCatch({
      assertthat::assert_that(is_data_frame(df,
        exact_colnames = c("foo", "bar", "baz")))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0("Error: df must be a data frame with exact colnames ",
      "'foo', 'bar', 'baz'. ",
      "Got: structure(list(foo = c(1, 2, 3), bar = c(3, 4, 5)), ",
      "class = \"data.frame\", row.names = c(NA, -3L))\n")
    )

  err <- tryCatch({
      assertthat::assert_that(is_data_frame(df,
        exact_colnames = c("foo", "bar", "baz"),
        exact_rownames = c("one", "two")))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0("Error: df must be a data frame with exact rownames ",
      "'one', 'two' and with exact colnames 'foo', 'bar', 'baz'. ",
      "Got: structure(list(foo = c(1, 2, 3), bar = c(3, 4, 5)), ",
      "class = \"data.frame\", row.names = c(NA, -3L))\n")
    )
})

test_that("data_frames_same_colnames", {
  df1 <- data.frame(foo=c(1,2,3), bar=c(1,2,3))
  df2 <- data.frame(foo=c(1,2,3))
  df3 <- data.frame(foo=c(1,2,3), bar=c(1,2,3), baz=c(1,2,3))

  expect_true(data_frames_same_colnames(df1, df1))
  expect_false(data_frames_same_colnames(df1, df2))
  expect_false(data_frames_same_colnames(df1, df3))

  err <- tryCatch({
      assertthat::assert_that(data_frames_same_colnames(df1, df2))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: data frames df1 and df2 must have the exact same column names.\n")

})
