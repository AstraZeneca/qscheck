#' Check if the passed entity is a matrix
#'
#' @param value the value to check
#' @param exact_num_rows If specified, the matrix must have the specified
#'        exact number of rows
#' @param exact_num_cols If specified, the matrix must have the specified
#'        exact number of cols
#' @param allow_null If TRUE, allow NULL as a value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_matrix(value))
#' # For check
#' if (qscheck::is_matrix(value)) {}
#' }
#'
#' @concept matrix
#' @export
is_matrix <- function(
    value, exact_num_rows = NULL, exact_num_cols = NULL, allow_null = FALSE) {
  res <- inspect_matrix(
    value, exact_num_rows, exact_num_cols, allow_null
  )
  return(res$valid)
}
assertthat::on_failure(is_matrix) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_num_rows <- callget(call, env, "exact_num_rows", NULL)
  exact_num_cols <- callget(call, env, "exact_num_cols", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_matrix(
    value, exact_num_rows, exact_num_cols, allow_null
  )
  return(paste0(
    deparse(call$value),
    snippet_must_be("matrix"),
    snippet_exact_matrix_dimension(exact_num_rows, exact_num_cols),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}

inspect_matrix <- function(
  value, exact_num_rows = NULL, exact_num_cols = NULL, allow_null = FALSE) {

  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (!is_s3_instance(value, "matrix")) {
    return(failure("Passed value is not a matrix"))
  }

  if (!is.null(exact_num_rows) && nrow(value) != exact_num_rows) {
    return(failure(
      paste0("Passed matrix has ", nrow(value), " rows")
    ))
  }

  if (!is.null(exact_num_cols) && ncol(value) != exact_num_cols) {
    return(failure(
      paste0("Passed matrix has ", ncol(value), " columns")
    ))
  }

  return(success())
}

#' Check if the passed entity is a square matrix
#'
#' @param value the value to check
#' @param exact_dimension If specified, the matrix must have the specified
#'        exact dimension
#' @param allow_null If TRUE, allow NULL as a value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_square_matrix(value))
#' # For check
#' if (qscheck::is_square_matrix(value)) {}
#' }
#'
#' @concept matrix
#' @export
is_square_matrix <- function(
    value, exact_dimension = NULL, allow_null = FALSE) {
  res <- inspect_square_matrix(
    value, exact_dimension, allow_null
  )
  return(res$valid)
}
assertthat::on_failure(is_square_matrix) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_dimension <- callget(call, env, "exact_dimension", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_square_matrix(
    value, exact_dimension, allow_null
  )
  return(paste0(
    deparse(call$value),
    snippet_must_be("square matrix"),
    snippet_exact_matrix_dimension(exact_dimension, exact_dimension),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}
inspect_square_matrix <- function(
    value, exact_dimension = NULL, allow_null = FALSE) {

  res <- inspect_matrix(
    value,
    exact_num_rows = exact_dimension,
    exact_num_cols = exact_dimension,
    allow_null = allow_null
  )

  if (!res$valid) {
    return(res)
  }

  # We might have NULL here that passed, but we can't check if it's square.
  # If we got to here, it means that inspect_matrix was successful because
  # allow_null was specified, and the value is actually NULL.
  if (is.null(value)) {
    return(success())
  }

  if (nrow(value) != ncol(value)) {
    return(failure(
      paste0(
        "Passed non-square matrix with dimensions (", nrow(value), ", ",
        ncol(value), ")"
      ))
    )
  }
  return(success())
}

#' Check if the passed entity is a diagonal matrix. Elements outside of
#' the diagonal are checked against a given tolerance.
#'
#' @param value The value to check.
#' @param exact_dimension If specified, the matrix must have the specified
#'        exact dimension.
#' @param allow_null If TRUE, allow NULL as a value.
#' @param tol The tolerance to verify if the off-diagonal elements are
#'            zero. Default is sqrt(.Machine$double.eps).
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_diagonal_matrix(value))
#' # For check
#' if (qscheck::is_diagonal_matrix(value)) {}
#' }
#'
#' @concept matrix
#' @export
is_diagonal_matrix <- function(
    value,
    exact_dimension = NULL,
    allow_null = FALSE,
    tol = sqrt(.Machine$double.eps)
    ) {

  res <- inspect_diagonal_matrix(
    value, exact_dimension, allow_null,
    tol
  )
  return(res$valid)
}
assertthat::on_failure(is_diagonal_matrix) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_dimension <- callget(call, env, "exact_dimension", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  tol <- callget(call, env, "tol", sqrt(.Machine$double.eps))

  res <- inspect_diagonal_matrix(
    value, exact_dimension = exact_dimension, allow_null = allow_null, tol = tol
  )
  return(paste0(
    deparse(call$value),
    snippet_must_be("diagonal matrix"),
    snippet_exact_matrix_dimension(exact_dimension, exact_dimension),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}
inspect_diagonal_matrix <- function(
    value, exact_dimension = NULL, allow_null = FALSE,
    tol = sqrt(.Machine$double.eps)
    ) {

  res <- inspect_square_matrix(
    value,
    exact_dimension = exact_dimension,
    allow_null = allow_null
  )

  if (!res$valid) {
    return(res)
  }

  if (is.null(value)) {
    return(success())
  }

  off_diag_mask <- !diag(nrow(value))
  if (any(is.na(value[off_diag_mask]))) {
    return(failure("Passed matrix cannot contain non-diagonal NAs"))
  }

  if (any(abs(value[off_diag_mask]) > tol)) {
    return(failure("Passed matrix has non-zero off-diagonal values"))
  }

  return(success())
}

#' Check if the passed entity is an identity matrix
#'
#' @param value the value to check
#' @param exact_dimension If specified, the matrix must have the specified
#'        exact dimension
#' @param allow_null If TRUE, allow NULL as a value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_identity_matrix(value))
#' # For check
#' if (qscheck::is_identity_matrix(value)) {}
#' }
#'
#' @concept matrix
#' @export
is_identity_matrix <- function(
    value, exact_dimension = NULL, allow_null = FALSE,
    tol = sqrt(.Machine$double.eps)
    ) {
  res <- inspect_identity_matrix(
    value, exact_dimension, allow_null, tol
  )
  return(res$valid)
}
assertthat::on_failure(is_identity_matrix) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  exact_dimension <- callget(call, env, "exact_dimension", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  tol <- callget(call, env, "tol", sqrt(.Machine$double.eps))

  res <- inspect_identity_matrix(
    value, exact_dimension, allow_null, tol
  )
  return(paste0(
    deparse(call$value),
    snippet_must_be("identity matrix"),
    snippet_exact_matrix_dimension(exact_dimension, exact_dimension),
    snippet_null(allow_null),
    ". ", res$reason
  ))
}
inspect_identity_matrix <- function(
    value, exact_dimension = NULL, allow_null = FALSE,
    tol = sqrt(.Machine$double.eps)
  ) {

  res <- inspect_diagonal_matrix(
    value,
    exact_dimension = exact_dimension,
    allow_null = allow_null,
    tol = tol
  )

  if (!res$valid) {
    return(res)
  }

  if (is.null(value)) {
    return(success())
  }

  diagonal <- diag(value)
  if (any(is.na(diagonal))) {
    return(failure("Passed matrix cannot contain NAs on the diagonal"))
  }

  if (any(abs(diagonal - 1.0) > tol)) {
    return(failure("Passed matrix have values other than 1 on the diagonal"))
  }

  return(success())
}

#' Check if the passed entities are matrixes with the same dimensionality
#'
#' @param m1 The first matrix
#' @param m2 The second matrix
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::matrixes_same_dims(m1, m2))
#' # For check
#' if (qscheck::matrixes_same_dims(m1, m2)) {}
#' }
#'
#' @concept matrix
#' @export
matrixes_same_dims <- function(m1, m2) {
  res <- inspect_matrixes_same_dims(m1, m2)
  return(res$valid)
}
assertthat::on_failure(matrixes_same_dims) <- function(call, env) {
  m1 <- callget(call, env, "m1", NULL)
  m2 <- callget(call, env, "m2", NULL)
  res <- inspect_matrixes_same_dims(m1, m2)
  return(paste0(
    deparse(call$m1), " and ", deparse(call$m2),
    " must be matrixes with the exact same dimensions. ",
    res$reason))
}
inspect_matrixes_same_dims <- function(m1, m2) {
  if (!is_matrix(m1)) {
    return(failure("The first element is not a matrix"))
  }
  if (!is_matrix(m2)) {
    return(failure("The second element is not a matrix"))
  }
  if (ncol(m1) != ncol(m2)) {
    return(failure(
      paste0(
        "The first matrix has ", ncol(m1), " columns ",
        "and the second has ", ncol(m2), " columns")
      )
    )
  }
  if (nrow(m1) != nrow(m2)) {
    return(failure(
      paste0(
        "The first matrix has ", nrow(m1), " rows ",
        "and the second has ", nrow(m2), " rows")
      )
    )
  }
  return(success())
}

#' Check if the passed entities are matrixes that can multiply together
#'
#' @param m1 The first matrix
#' @param m2 The second matrix
#' @param result_num_rows The expected number of rows of the result. Optional.
#' @param result_num_cols The expected number of cols of the result. Optional.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::matrixes_can_multiply(m1, m2))
#' # For check
#' if (qscheck::matrixes_can_multiply(m1, m2)) {}
#' }
#'
#' @concept matrix
#' @export
matrixes_can_multiply <- function(
    m1, m2, result_num_rows = NULL, result_num_cols = NULL
    ) {
  res <- inspect_matrixes_can_multiply(m1, m2, result_num_rows, result_num_cols)
  return(res$valid)
}
assertthat::on_failure(matrixes_can_multiply) <- function(call, env) {
  m1 <- callget(call, env, "m1", NULL)
  m2 <- callget(call, env, "m2", NULL)
  result_num_rows <- callget(call, env, "result_num_rows", NULL)
  result_num_cols <- callget(call, env, "result_num_cols", NULL)
  res <- inspect_matrixes_can_multiply(m1, m2, result_num_rows, result_num_cols)

  return(paste0(
    deparse(call$m1), " and ", deparse(call$m2),
    " must be matrixes that can multiply",
    snippet_matmult_result(result_num_rows, result_num_cols),
    ". ",
    res$reason
    )
  )
}
inspect_matrixes_can_multiply <- function(
    m1, m2, result_num_rows, result_num_cols) {
  if (!is_matrix(m1)) {
    return(failure("The first element is not a matrix"))
  }
  if (!is_matrix(m2)) {
    return(failure("The second element is not a matrix"))
  }
  if (ncol(m1) != nrow(m2)) {
    return(failure(
      paste0(
        "The first matrix has ", ncol(m1), " columns ",
        "and the second has ", nrow(m2), " rows")
      )
    )
  }

  mult_result_str <- paste0(
    "The multiplication would give ", nrow(m1), " rows and ",
    ncol(m2), " columns but ",
    snippet_matmult_expected(result_num_rows, result_num_cols),
    " are expected"
  )

  if (!is.null(result_num_rows) && nrow(m1) != result_num_rows) {
    return(failure(mult_result_str))
  }

  if (!is.null(result_num_cols) && ncol(m2) != result_num_cols) {
    return(failure(mult_result_str))
  }
  return(success())
}
