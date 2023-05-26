test_that("variableBeingDataFrame", {
  df <- data.frame(
      foo = c(1, 2, 3),
      bar = c(3, 4, 5)
  )
  empty <- data.frame()
  expect_true(is_data_frame(df))
  expect_true(is_data_frame(df, required_colnames = c("foo")))
  expect_false(is_data_frame(df, required_colnames = c("baz")))
  expect_false(is_data_frame(df, exact_colnames = c("foo", "bar", "baz")))
  expect_false(is_data_frame(df, exact_colnames = c("foo")))
  expect_true(is_data_frame(df, exact_colnames = c("foo", "bar")))
  expect_false(is_data_frame(NA_real_))

  expect_error({
      assertthat::assert_that(is_data_frame(df, required_colnames = c("baz")))
    },
    paste0(
      "df must be a data frame with required colnames 'baz'\\. ",
      "'baz' is not in the colnames 'foo', 'bar'"
    )
  )

  expect_error({
      assertthat::assert_that(is_data_frame(df,
        exact_colnames = c("foo", "bar", "baz")))
    },
    paste0("df must be a data frame with exact colnames ",
      "'foo', 'bar', 'baz'\\. passed data frame has a different ",
      "number of colnames"
    )
  )

  expect_error({
      assertthat::assert_that(is_data_frame(df,
        exact_colnames = c("foo", "bar", "baz"),
        exact_rownames = c("one", "two")))
    },
    paste0("df must be a data frame with exact rownames ",
      "'one', 'two' with exact colnames 'foo', 'bar', 'baz'\\. ",
      "passed data frame has a different number of rownames"
    )
  )
})

test_that("twoDataFramesHaveExactColumnNames", {
  df1 <- data.frame(foo = c(1, 2, 3), bar = c(1, 2, 3))
  df2 <- data.frame(foo = c(1, 2, 3))
  df3 <- data.frame(foo = c(1, 2, 3), bar = c(1, 2, 3), baz = c(1, 2, 3))

  expect_true(data_frames_same_colnames(df1, df1))
  expect_false(data_frames_same_colnames(df1, df2))
  expect_false(data_frames_same_colnames(df1, df3))

  expect_error({
      assertthat::assert_that(data_frames_same_colnames(df1, df2))
    },
    "data frames df1 and df2 must have the exact same column names\\."
  )

})
