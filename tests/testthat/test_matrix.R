test_that("is_matrix", {
  expect_true(is_matrix(matrix(nrow = 3, ncol = 3)))
  expect_true(is_matrix(matrix(nrow = 3, ncol = 3), exact_num_rows = 3))
  expect_true(is_matrix(matrix(nrow = 3, ncol = 3), exact_num_cols = 3))
  expect_true(is_matrix(NULL, allow_null = TRUE))
  expect_false(is_matrix(c(FALSE, FALSE)))
  expect_false(is_matrix(""))
  foo <- -1.5

  expect_error(
    assertthat::assert_that(is_matrix(foo)),
    "foo must be a matrix. Passed value is not a matrix")

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(is_matrix(foo, exact_num_rows = 5)),
    "foo must be a matrix with exactly 5 rows. Passed matrix has 3 rows")

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(is_matrix(foo, exact_num_cols = 5)),
    "foo must be a matrix with exactly 5 columns. Passed matrix has 3 columns")

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(
      is_matrix(foo, exact_num_rows = 4, exact_num_cols = 5)
    ),
    paste0(
      "foo must be a matrix with exactly 4 rows and 5 columns. ",
      "Passed matrix has 3 rows"
    )
  )
  expect_error(
    assertthat::assert_that(
      is_matrix(foo, exact_num_rows = 4, exact_num_cols = 5, allow_null = TRUE)
    ),
    paste0(
      "foo must be a matrix with exactly 4 rows and 5 columns or NULL. ",
      "Passed matrix has 3 rows"
    )
  )
})

test_that("is_square_matrix", {
  expect_true(is_square_matrix(matrix(nrow = 3, ncol = 3)))
  expect_false(is_square_matrix(matrix(nrow = 4, ncol = 3)))
  expect_true(is_square_matrix(matrix(nrow = 3, ncol = 3), exact_dimension = 3))
  expect_false(is_square_matrix(matrix(nrow = 3, ncol = 3), exact_dimension = 4))
  expect_true(is_square_matrix(NULL, allow_null = TRUE))
  expect_false(is_square_matrix(c(FALSE, FALSE)))
  expect_false(is_square_matrix(""))
  foo <- -1.5

  expect_error(
    assertthat::assert_that(is_square_matrix(foo)),
    "foo must be a square matrix. Passed value is not a matrix")

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(is_square_matrix(foo, exact_dimension = 5)),
    paste0(
      "foo must be a square matrix with exactly 5 rows and 5 columns. ",
      "Passed matrix has 3 rows"
    )
  )

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(is_square_matrix(foo, exact_dimension = 5)),
    paste0(
      "foo must be a square matrix with exactly 5 rows and 5 columns. ",
      "Passed matrix has 3 rows"))

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(
      is_square_matrix(foo, exact_dimension = 4)
    ),
    paste0(
      "foo must be a square matrix with exactly 4 rows and 4 columns. ",
      "Passed matrix has 3 rows"
    )
  )
  expect_error(
    assertthat::assert_that(
      is_square_matrix(foo, exact_dimension = 4, allow_null = TRUE)
    ),
    paste0(
      "foo must be a square matrix with exactly 4 rows and 4 columns or NULL. ",
      "Passed matrix has 3 rows"
    )
  )

  foo <- matrix(nrow = 4, ncol = 3)
  expect_error(
    assertthat::assert_that(
      is_square_matrix(foo)
    ),
    paste0(
      "foo must be a square matrix. Passed non-square matrix ",
      "with dimensions \\(4, 3\\)"
    )
  )
})