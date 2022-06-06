#' Checks if the passed entity is a data frame.
#'
#' @param df the entity that might be a data frame
#' @param exact_rownames vector of strings that, if passed, add the
#'                       requirement for the data frame to contain
#'                       *exactly* the named rows. Note that if a name
#'                       has NULL assigned to it, it counts as not present.
#' @param exact_colnames as required_rownames, but for column names.
#' @param required_rownames vector of strings that, if passed, add the
#'                      requirement for the data frame to contain *at least*
#'                      those named rows, respectively. Note that if a name
#'                      has NULL assigned to it, it counts as not present.
#'                      If exact_rownames is present, this option is ignored.
#' @param required_colnames as required_rownames, but for column names
#' @param allow_na if TRUE, NA is accepted as a valid value for df.
#'                 if FALSE (default) do not accept it.
#' @param allow_null if TRUE, NULL is accepted as a valid value.
#'                   If FALSE (default) do not accept it.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_data_frame(my_parameter))
#' # For check
#' if (qscheck::is_data_frame()) {}
#' }
#'
#' @export
is_data_frame <- function(df,
                          exact_rownames = NULL,
                          exact_colnames = NULL,
                          required_rownames = NULL,
                          required_colnames = NULL,
                          allow_na = FALSE,
                          allow_null = FALSE) {

  if (is_na_value(df) && allow_na) {
    return(TRUE)
  }

  if (is.null(df) && allow_null) {
    return(TRUE)
  }

  if (!is.data.frame(df)) {
    return(FALSE)
  }

  if (!is.null(exact_rownames)) {
    if (length(exact_rownames) != length(rownames(df))) {
      return(FALSE)
    }
    for (rowname in exact_rownames) {
      if (!(rowname %in% rownames(df))) {
        return(FALSE)
      }
    }
  } else if (!is.null(required_rownames)) {
    for (rowname in required_rownames) {
      if (!(rowname %in% rownames(df))) {
        return(FALSE)
      }
    }
  }

  if (!is.null(exact_colnames)) {
    if (length(exact_colnames) != length(colnames(df))) {
      return(FALSE)
    }
    for (colname in exact_colnames) {
      if (!(colname %in% colnames(df))) {
        return(FALSE)
      }
    }
  } else if (!is.null(required_colnames)) {
    for (colname in required_colnames) {
      if (!(colname %in% colnames(df))) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_data_frame) <- function(call, env) {
  allow_na_msg <- ""
  if (!is.null(call$allow_na)) {
    if (eval(call$allow_na)) {
      allow_na_msg <- " or NA"
    }
  }

  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null)) {
      allow_null_msg <- " or NULL"
    }
  }

  base_msg <- paste0(deparse(call$df), " must be a data frame")
  exact_colnames_msg <- NULL
  exact_rownames_msg <- NULL
  required_rownames_msg <- NULL
  required_colnames_msg <- NULL
  if (!is.null(call$exact_colnames)) {
    exact_colnames_msg <- paste0(" with exact colnames '",
                  paste0(
                    eval(call$exact_colnames, env),
                    collapse = "', '"
                    ),
                  "'"
                  )
  } else if (!is.null(call$required_colnames)) {
    required_colnames_msg <- paste0(" with required colnames '",
                  paste0(
                    eval(call$required_colnames, env),
                    collapse = "', '"
                    ),
                  "'"
                  )
  }

  if (!is.null(call$exact_rownames)) {
    exact_rownames_msg <- paste0(" with exact rownames '",
                  paste0(
                    eval(call$exact_rownames, env),
                    collapse = "', '"
                    ),
                  "'"
                  )
  } else if (!is.null(call$required_rownames)) {
    required_rownames_msg <- paste0(" with required rownames '",
                  paste0(
                    eval(call$required_rownames, env),
                    collapse = "', '"
                    ),
                  "'"
                  )
  }


  msg <- paste0(
    base_msg,
    paste0(
      c(exact_rownames_msg,
        exact_colnames_msg,
        required_rownames_msg,
        required_colnames_msg), collapse = " and"
      ),
    allow_na_msg,
    allow_null_msg,
    ". Got: ",
    paste0(deparse(eval(call$df, env)), collapse = ""))
  return(msg)
}

#' Checks if two data frames have the exact same column names
#'
#' @param df1 the first data frame
#' @param df2 the first data frame
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::data_frames_same_colnames(df1, df2))
#' # For check
#' if (qscheck::data_frames_same_colnames(df1, df2)) {}
#' }
#'
#' @export
data_frames_same_colnames <- function(df1, df2) {
  if (!is_data_frame(df1)) {
    return(FALSE)
  }

  if (!is_data_frame(df2, exact_colnames = colnames(df1))) {
    return(FALSE)
  }

  return(TRUE)
}

assertthat::on_failure(data_frames_same_colnames) <- function(call, env) {
  return(paste0(
    "data frames ", deparse(call$df1), " and ", deparse(call$df2),
    " must have the exact same column names."))
}
