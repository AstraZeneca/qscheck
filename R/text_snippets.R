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

  article <- "a"
  if (substr(type, 1, 1) %in% c("a", "i", "e", "o", "u")) {
    article <- "an"
  }
  return(paste0(" must be ", article, " ", type))
}

snippet_na_values <- function(allow_na_values) {
  msg <- ""
  if (!allow_na_values) {
    msg <- paste0(" with no NAs")
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

snippet_numerical_range <- function(min = NULL, max = NULL) {
  msg <- ""
  if (!is.null(min) || !is.null(max)) {
    msg <- " in the range "
    if (is.null(min)) {
      msg <- paste0(msg, "(-inf, ")
    } else {
      msg <- paste0(msg, "[", min, ", ")
    }

    if (is.null(max)) {
      msg <- paste0(msg, "inf)")
    } else {
      msg <- paste0(msg, max, "]")
    }
  }
  return(msg)
}

snippet_degenerate <- function(allow_degenerate) {
  msg <- ""
  if (!allow_degenerate) {
    msg <- " non-degenerate"
  }
  return(msg)
}

snippet_names <- function(required_names = NULL) {
  msg <- ""
  if (!is.null(required_names)) {
    msg <- paste0(" with at least names ", flatten_vector(required_names))
  }
  return(msg)
}

flatten_vector <- function(vector) {
  msg <- paste0(
    "('", paste0(vector, collapse = "', '"), "')"
  )

  return(msg)
}
