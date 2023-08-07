test_that("variableIsAMatrix", {
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

test_that("passedEntityIsSquaredMatrix", {
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

test_that("passedEntityIsDiagonalMatrix", {
  m1 <- matrix(c(1, 0, 0, 0, 2, 0, 0, 0, 3), 3, 3)
  m2 <- matrix(c(1, 2, 0, 0, 0, 0, 0, 0, 3), 3, 3)
  m3 <- matrix(c(1, 0, NA_real_, 0, 2, 0, 0, 0, 3), 3, 3)
  m4 <- matrix(c(1, 0, 1.0e-7, 0, 2, 0, 0, 0, 3), 3, 3)
  m5 <- matrix(c(1, 0, 1.0e-8, 0, 2, 0, 0, 0, 3), 3, 3)

  expect_false(is_diagonal_matrix(matrix(nrow = 3, ncol = 3)))
  expect_false(is_diagonal_matrix(matrix(nrow = 4, ncol = 3)))
  expect_true(is_diagonal_matrix(m1))
  expect_true(is_diagonal_matrix(m1, exact_dimension = 3))
  expect_false(is_diagonal_matrix(m1, exact_dimension = 4))
  expect_false(is_diagonal_matrix(m2))

  expect_false(is_diagonal_matrix(NULL))
  expect_true(is_diagonal_matrix(NULL, allow_null = TRUE))

  expect_false(is_diagonal_matrix(c(FALSE, FALSE)))
  expect_false(is_diagonal_matrix(""))

  foo <- -1.5

  expect_error(
    assertthat::assert_that(is_diagonal_matrix(foo)),
    "foo must be a diagonal matrix. Passed value is not a matrix")

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(is_diagonal_matrix(foo, exact_dimension = 5)),
    paste0(
      "foo must be a diagonal matrix with exactly 5 rows and 5 columns. ",
      "Passed matrix has 3 rows"
    )
  )

  foo <- matrix(nrow = 4, ncol = 3)
  expect_error(
    assertthat::assert_that(
      is_diagonal_matrix(foo)
    ),
    paste0(
      "foo must be a diagonal matrix. Passed non-square matrix ",
      "with dimensions \\(4, 3\\)"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_diagonal_matrix(m2)
    ),
    paste(
      "m2 must be a diagonal matrix. Passed matrix has non-zero off-diagonal values"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_diagonal_matrix(m3)
    ),
    paste0(
      "m3 must be a diagonal matrix\\. ",
      "Passed matrix cannot contain non-diagonal NAs"
    )
  )

 expect_error(
    assertthat::assert_that(is_diagonal_matrix(m4)),
    paste0(
      "m4 must be a diagonal matrix\\. Passed matrix has non-zero ",
      "off-diagonal values"
    )
  )

  expect_true(is_diagonal_matrix(m4, tol = 1e-6))
  expect_true(is_diagonal_matrix(m5))
})

test_that("passedEntityIsIdentityMatrix", {
  m1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1), 3, 3)
  m3 <- matrix(c(1, 0, 0, 0, NA, 0, 0, 0, 1), 3, 3)
  m4 <- matrix(c(1, 0, 0, 0, 1 + 1e-7, 0, 0, 0, 1), 3, 3)
  m5 <- matrix(c(1, 0, 0, 0, 1 + 1e-8, 0, 0, 0, 1), 3, 3)

  expect_false(is_identity_matrix(matrix(nrow = 3, ncol = 3)))
  expect_false(is_identity_matrix(matrix(nrow = 4, ncol = 3)))
  expect_false(is_identity_matrix(matrix(nrow = 3, ncol = 3), exact_dimension = 3))
  expect_false(is_identity_matrix(matrix(nrow = 3, ncol = 3), exact_dimension = 4))
  expect_true(is_identity_matrix(m1))
  expect_true(is_identity_matrix(m1, exact_dimension = 3))
  expect_false(is_identity_matrix(m1, exact_dimension = 4))
  expect_false(is_identity_matrix(m2))
  expect_true(is_identity_matrix(NULL, allow_null = TRUE))
  expect_false(is_identity_matrix(c(FALSE, FALSE)))
  expect_false(is_identity_matrix(""))

  foo <- -1.5

  expect_error(
    assertthat::assert_that(is_identity_matrix(foo)),
    "foo must be an identity matrix. Passed value is not a matrix")

  foo <- matrix(nrow = 3, ncol = 3)
  expect_error(
    assertthat::assert_that(is_identity_matrix(foo, exact_dimension = 5)),
    paste0(
      "foo must be an identity matrix with exactly 5 rows and 5 columns. ",
      "Passed matrix has 3 rows"
    )
  )

  foo <- matrix(nrow = 4, ncol = 3)
  expect_error(
    assertthat::assert_that(
      is_identity_matrix(foo)
    ),
    paste0(
      "foo must be an identity matrix\\. Passed non-square matrix ",
      "with dimensions \\(4, 3\\)"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_identity_matrix(m2)
    ),
    paste0(
      "m2 must be an identity matrix\\. ",
      "Passed matrix has non-zero off-diagonal values"
    )
  )

  expect_error(
    assertthat::assert_that(
      is_identity_matrix(m3)
    ),
    paste0(
      "m3 must be an identity matrix. ",
      "Passed matrix cannot contain NAs on the diagonal"
    )
  )

  expect_error(
    assertthat::assert_that(is_identity_matrix(m4)),
    paste0(
      "m4 must be an identity matrix\\. ",
      "Passed matrix have values other than 1 on the diagonal"
    )
  )

  expect_true(is_identity_matrix(m4, tol = 1e-6))
  expect_true(is_diagonal_matrix(m5))

})

test_that("matrixesSameDimsNotMatrixes", {
  m1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1), 3, 3)
  expect_false(matrixes_same_dims(1, 3))
  expect_false(matrixes_same_dims(m1, 3))
  expect_false(matrixes_same_dims(1, m2))
  expect_error(assert(matrixes_same_dims(1, 3)),
    paste0(
      "1 and 3 must be matrixes with the exact same dimensions\\. ",
      "The first element is not a matrix"
    )
  )
  expect_error(assert(matrixes_same_dims(m1, 3)),
    paste0(
      "m1 and 3 must be matrixes with the exact same dimensions\\. ",
      "The second element is not a matrix"
    )
  )
  expect_error(assert(matrixes_same_dims(1, m2)),
    paste0(
      "1 and m2 must be matrixes with the exact same dimensions\\. ",
      "The first element is not a matrix"
    )
  )
})

test_that("matrixesSameDimsDifferentDimensions", {
  m1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 4, 3)
  m3 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 3, 4)
  expect_false(matrixes_same_dims(m1, m2))
  expect_false(matrixes_same_dims(m1, m3))
  expect_error(assert(matrixes_same_dims(m1, m2)),
    paste0(
      "m1 and m2 must be matrixes with the exact same dimensions. ",
      "The first matrix has 3 rows and the second has 4 rows"
    ))
  expect_error(assert(matrixes_same_dims(m1, m3)),
    paste0(
    "m1 and m3 must be matrixes with the exact same dimensions. ",
    "The first matrix has 3 columns and the second has 4 columns"
  ))
})

test_that("matrixesSameDimsSameDimensions", {
  m1 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 4, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 4, 3)

  expect_true(matrixes_same_dims(m1, m2))
})

test_that("matrixesMultNotMatrixes", {
  m1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1), 3, 3)
  expect_false(matrixes_can_multiply(1, 3))
  expect_false(matrixes_can_multiply(m1, 3))
  expect_false(matrixes_can_multiply(1, m2))
  expect_error(assert(matrixes_can_multiply(1, 3)),
    paste0(
      "1 and 3 must be matrixes that can multiply\\. ",
      "The first element is not a matrix"
    )
  )
  expect_error(assert(matrixes_can_multiply(m1, 3)),
    paste0(
      "m1 and 3 must be matrixes that can multiply\\. ",
      "The second element is not a matrix"
    )
  )
  expect_error(assert(matrixes_can_multiply(1, m2)),
    paste0(
      "1 and m2 must be matrixes that can multiply\\. ",
      "The first element is not a matrix"
    )
  )

})

test_that("matrixesMultNotCompatible", {
  m1 <- matrix(rep(1, 12), 4, 3)
  m2 <- matrix(rep(1, 24), 4, 6)
  expect_false(matrixes_can_multiply(m1, m2))
  expect_error(assert(matrixes_can_multiply(m1, m2)),
    paste0(
      "m1 and m2 must be matrixes that can multiply\\. ",
      "The first matrix has 3 columns and the second has 4 rows"
    )
  )
})

test_that("matrixesMultCompatible", {
  m1 <- matrix(rep(1, 12), 4, 3)
  m2 <- matrix(rep(1, 24), 3, 6)
  expect_true(matrixes_can_multiply(m1, m2))
})

test_that("matrixesMultIncorrectResult", {
  m1 <- matrix(rep(1, 12), 4, 3)
  m2 <- matrix(rep(1, 24), 3, 6)
  expect_true(matrixes_can_multiply(m1, m2))
  expect_false(matrixes_can_multiply(m1, m2, result_num_cols = 5))
  expect_false(matrixes_can_multiply(m1, m2, result_num_rows = 2))
  expect_false(
    matrixes_can_multiply(m1, m2, result_num_rows = 2, result_num_cols = 5)
  )
  expect_true(matrixes_can_multiply(
    m1, m2, result_num_rows = 4, result_num_cols = 6)
  )
  expect_error(
    assert(matrixes_can_multiply(m1, m2, result_num_cols = 5)),
    paste0(
      "m1 and m2 must be matrixes that can multiply to give a result ",
      "with exactly 5 columns\\. The multiplication would give ",
      "4 rows and 6 columns but 5 columns are expected"
    )
  )
  expect_error(
    assert(matrixes_can_multiply(m1, m2, result_num_rows = 2)),
    paste0(
      "m1 and m2 must be matrixes that can multiply to give a result ",
      "with exactly 2 rows\\. The multiplication would give ",
      "4 rows and 6 columns but 2 rows are expected"
    )
  )
  expect_error(
    assert(
      matrixes_can_multiply(m1, m2, result_num_rows = 2, result_num_cols = 5)
    ),
    paste0(
      "m1 and m2 must be matrixes that can multiply to give a result ",
      "with exactly 2 rows and 5 columns\\. The multiplication would give ",
      "4 rows and 6 columns but 2 rows and 5 columns are expected"
    )
  )
})

test_that("matrixesSameRows", {
  v <- 3
  m1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 4, 3)
  m3 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 3, 4)
  expect_true(matrixes_same_rows(m1, m3))
  expect_false(matrixes_same_rows(m1, m2))
  expect_false(matrixes_same_rows(v, m2))
  expect_false(matrixes_same_rows(m1, v))
  expect_error(assert(matrixes_same_rows(v, m2)),
    paste0(
      "v and m2 must be matrixes with the exact same number of rows. ",
      "The first element is not a matrix"
    ))
  expect_error(assert(matrixes_same_rows(m1, m2)),
    paste0(
      "m1 and m2 must be matrixes with the exact same number of rows. ",
      "The first matrix has 3 rows and the second has 4 rows"
    ))
})

test_that("matrixesSameCols", {
  v <- 3
  m1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  m2 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 4, 3)
  m3 <- matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 3, 4)
  expect_true(matrixes_same_cols(m1, m2))
  expect_false(matrixes_same_cols(m1, m3))
  expect_false(matrixes_same_cols(v, m2))
  expect_false(matrixes_same_cols(m1, v))
  expect_error(assert(matrixes_same_cols(v, m2)),
    paste0(
      "v and m2 must be matrixes with the exact same number of columns. ",
      "The first element is not a matrix"
    ))
  expect_error(assert(matrixes_same_cols(m1, m3)),
    paste0(
      "m1 and m3 must be matrixes with the exact same number of columns. ",
      "The first matrix has 3 columns and the second has 4 columns"
    ))
})
