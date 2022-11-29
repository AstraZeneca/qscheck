snippet_null <- function(allow_null) {
  allow_null_msg <- ""
  if (allow_null) {
    allow_null_msg <- " or NULL"
  }
  return(allow_null_msg)
}

snippet_na <- function(allow_na) {
  allow_na_msg <- ""
  if (allow_na) {
    allow_na_msg <- " or NA"
  }
  return(allow_na_msg)

}

snippet_length <- function(
    exact_length = NULL, min_length = NULL, max_length = NULL
  ) {
  msg <- ""
  if (!is.null(exact_length)) {
    msg <- paste0(" of exact length ", exact_length)
  } else if (!is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(
      " of length between ", min_length, " and ", max_length, " inclusive"
    )
  } else if (is.null(min_length) && !is.null(max_length)) {
    msg <- paste0(" of length not greater than ", max_length)
  } else if (!is.null(min_length) && is.null(max_length)) {
    msg <- paste0(" of length not less than ", min_length)
  }

  return(msg)
}

snippet_must_be <- function(type) {
  return(paste0(" must be a ", type))
}

snippet_na_values <- function(allow_na_values) {
  msg <- ""
  if (!allow_na_values) {
    msg <- paste0(" with no NA values")
  }
  return(msg)
}

snippet_exact_levels <- function(exact_levels) {
  msg <- ""
  if (!is.null(exact_levels)) {
    msg <- paste0(" with exact levels ", flatten_vector(exact_levels))
  }
  return(msg)
}

flatten_vector <- function(vector) {
  msg <- paste0(
    "('", paste0(vector, collapse = "', '"), "')"
  )

  return(msg)
}
