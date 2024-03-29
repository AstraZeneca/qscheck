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

snippet_must_be <- function(what, article = TRUE) {

  article_str <- ""
  if (article) {
    article_str <- "a "
    if (substr(what, 1, 1) %in% c("a", "i", "e", "o", "u")) {
      article_str <- "an "
    }
  }

  return(paste0(" must be ", article_str, what))
}

snippet_na_values <- function(allow_na_values) {
  msg <- " or NAs"
  if (!allow_na_values) {
    msg <- paste0(" with no NAs")
  }
  return(msg)
}

snippet_exact_levels <- function(exact_levels) {
  msg <- ""
  if (!is.null(exact_levels)) {
    msg <- paste0(" with exact levels ",
      flatten_vector(exact_levels, parenthesis = TRUE))
  }
  return(msg)
}

snippet_numerical_range <- function(
    min = NULL, max = NULL, inclusive_min = TRUE, inclusive_max = TRUE
  ) {
  if (is.null(min)) {
    min <- -Inf
  }
  if (is.null(max)) {
    max <- Inf
  }

  is_neg_infinite <- function(x) {
    is.infinite(x) && x < 0
  }
  is_pos_infinite <- function(x) {
    is.infinite(x) && x > 0
  }

  if (is_neg_infinite(min) && is_pos_infinite(max)) {
    return("")
  }

  msg <- " in the range "
  if (is.infinite(min)) {
    msg <- paste0(msg, "(", min, ", ")
  } else {
    msg <- paste0(
      msg,
      ifelse(inclusive_min, "[", "("),
      min, ", ")
  }

  if (is.infinite(max)) {
    msg <- paste0(msg, max, ")")
  } else {
    msg <- paste0(
      msg, max,
      ifelse(inclusive_max, "]", ")"))
  }
  return(msg)
}

snippet_degenerate <- function(allow_degenerate) {
  msg <- " possibly degenerate"
  if (!allow_degenerate) {
    msg <- " non-degenerate"
  }
  return(msg)
}

snippet_uniform <- function(allow_uniform) {
  msg <- " possibly uniform"
  if (!allow_uniform) {
    msg <- " non-uniform"
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

snippet_not_empty <- function(allow_empty) {
  msg <- ""
  if (!allow_empty) {
    msg <- " non-empty"
  }
  return(msg)
}


snippet_occurrences <- function(
    exact_occurrences = NULL, min_occurrences = NULL, max_occurrences = NULL) {

  if (!is.null(exact_occurrences)) {
    msg <- paste0(" exactly ", exact_occurrences, " times")
  } else if (!is.null(min_occurrences) && !is.null(max_occurrences)) {
    msg <- paste0(" between ",
      min_occurrences,
      " and ",
      max_occurrences,
      " times inclusive"
    )
  } else if (is.null(min_occurrences) && !is.null(max_occurrences)) {
    msg <- paste0(
      " no more than ",
      max_occurrences,
      " times inclusive"
    )
  } else if (!is.null(min_occurrences) && is.null(max_occurrences)) {
    msg <- paste0(
      " no less than ",
      min_occurrences,
      " times inclusive"
    )
  }

  return(msg)
}


snippet_rowcolnames <- function(
    exact_rownames, exact_colnames, required_rownames, required_colnames
  ) {
  msg <- ""

  if (!is.null(exact_rownames)) {
    msg <- paste0(
      msg, " with exact rownames ", flatten_vector(exact_rownames)
    )
  } else if (!is.null(required_rownames)) {
    msg <- paste0(
      msg, " with required rownames ", flatten_vector(required_rownames)
    )
  }

  if (!is.null(exact_colnames)) {
    msg <- paste0(
      msg, " with exact colnames ", flatten_vector(exact_colnames)
    )
  } else if (!is.null(required_colnames)) {
    msg <- paste0(
      msg, " with required colnames ", flatten_vector(required_colnames)
    )
  }


  return(msg)

}

snippet_exact_matrix_dimension <- function(num_rows, num_cols) {
  if (is.null(num_rows) && is.null(num_cols)) {
    return("")
  }

  msg <- " with exactly "

  if (!is.null(num_rows)) {
    msg <- paste0(msg, num_rows, " rows")
    if (!is.null(num_cols)) {
      msg <- paste0(msg, " and ")
    }
  }

  if (!is.null(num_cols)) {
    msg <- paste0(msg, num_cols, " columns")
  }

  return(msg)
}

snippet_function_args <- function(num_args, args) {
  msg <- ""
  if (!is.null(num_args)) {
   msg <- paste0(msg, " with ", num_args, " arguments")
  }

  if (!is.null(args)) {
    msg <- paste0(msg, " with arguments named ", flatten_vector(args))
  }

  return(msg)
}

snippet_violator_indexes <- function(indexes) {
  howmany <- length(indexes)
  msg <- paste0(
    pluralize_if("Value", howmany),
    " at ",
    pluralize_if("position", howmany),
    " ",
    flatten_vector(indexes),
    " ",
    pluralize_if("is", howmany),
    " not complying with the requirement"
  )
  return(msg)
}

snippet_comparison <- function(operator, comparator) {
  if (identical(operator, base::`<`)) {
    msg <- "smaller than"
  } else if (identical(operator, base::`<=`)) {
    msg <- "smaller than or equal to"
  } else if (identical(operator, base::`>`)) {
    msg <- "greater than"
  } else if (identical(operator, base::`>=`)) {
    msg <- "greater than or equal to"
  } else {
    stop("Comparison failed for operator. This is a programming error.")
  }
  return(paste0(" ", msg, " ", comparator))
}

snippet_matmult_result <- function(result_num_rows, result_num_cols) {
  prefix <- " to give a result with exactly"

  expected <- snippet_matmult_expected(result_num_rows, result_num_cols)
  if (expected == "") {
    return("")
  }
  return(paste0(prefix, " ", expected))
}

snippet_matmult_expected <- function(result_num_rows, result_num_cols) {
  if (!is.null(result_num_rows) && !is.null(result_num_cols)) {
    return(paste0(
      result_num_rows, " rows and ", result_num_cols, " columns")
    )
  } else if (is.null(result_num_rows) && !is.null(result_num_cols)) {
    return(paste0(result_num_cols, " columns"))
  } else if (!is.null(result_num_rows) && is.null(result_num_cols)) {
    return(paste0(result_num_rows, " rows"))
  } else {
    return("")
  }

  # unreachable
  return("")
}
