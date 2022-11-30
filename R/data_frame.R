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
#' @concept data_frame
#' @export
is_data_frame <- function(df,
                          exact_rownames = NULL,
                          exact_colnames = NULL,
                          required_rownames = NULL,
                          required_colnames = NULL,
                          allow_na = FALSE,
                          allow_null = FALSE
                          ) {

  res <- inspect_data_frame(
    df,
    exact_rownames,
    exact_colnames,
    required_rownames,
    required_colnames,
    allow_na,
    allow_null
  )
  return(res$valid)

}
assertthat::on_failure(is_data_frame) <- function(call, env) {
  df <- callget(call, env, "df", NULL)
  exact_rownames <- callget(call, env, "exact_rownames", NULL)
  exact_colnames <- callget(call, env, "exact_colnames", NULL)
  required_rownames <- callget(call, env, "required_rownames", NULL)
  required_colnames <- callget(call, env, "required_colnames", NULL)
  allow_na <- callget(call, env, "allow_na", FALSE)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_data_frame(
    df,
    exact_rownames,
    exact_colnames,
    required_rownames,
    required_colnames
  )
  msg <- paste0(
    deparse(call$df),
    snippet_must_be("data frame"),
    snippet_rowcolnames(
      exact_rownames,
      exact_colnames,
      required_rownames,
      required_colnames
    ),
    snippet_na(allow_na),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}

inspect_data_frame <- function(
  df,
  exact_rownames = NULL,
  exact_colnames = NULL,
  required_rownames = NULL,
  required_colnames = NULL,
  allow_na = FALSE,
  allow_null = FALSE
) {

  if (is.null(df)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("passed value is NULL"))
    }
  }

  if (is_na_value(df)) {
    if (allow_na == TRUE) {
      return(success())
    } else {
      return(failure("passed value is NA"))
    }
  }

  if (!is.data.frame(df)) {
    return(failure("passed value is not a data frame"))
  }

  if (!is.null(exact_rownames)) {
    if (length(exact_rownames) != length(rownames(df))) {
      return(failure("passed data frame has a different number of rownames"))
    }
    for (rowname in exact_rownames) {
      if (!(rowname %in% rownames(df))) {
        return(
          failure(paste0(
            "'", rowname, "' is not in the rownames ",
            flatten_vector(rownames(df))
          ))
        )
      }
    }
  } else if (!is.null(required_rownames)) {
    for (rowname in required_rownames) {
      if (!(rowname %in% rownames(df))) {
        return(
          failure(paste0(
            "'", rowname, "' is not in the rownames ",
            flatten_vector(rownames(df))
          ))
        )
      }
    }
  }

  if (!is.null(exact_colnames)) {
    if (length(exact_colnames) != length(colnames(df))) {
      return(failure("passed data frame has a different number of colnames"))
    }
    for (colname in exact_colnames) {
      if (!(colname %in% colnames(df))) {
        return(
          failure(paste0(
            "'", colname, "' is not in the colnames ",
            flatten_vector(colnames(df))
          ))
        )
      }
    }
  } else if (!is.null(required_colnames)) {
    for (colname in required_colnames) {
      if (!(colname %in% colnames(df))) {
        return(
          failure(paste0(
            "'", colname, "' is not in the colnames ",
            flatten_vector(colnames(df))
          ))
        )
      }
    }
  }

  return(success())
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
#' @concept data_frame
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
