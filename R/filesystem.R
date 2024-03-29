#' Checks if the path refers to an existing file.
#' Note that this checks for presence of a file. It will fail for directories.
#'
#' @param path the path to check
#' @param allow_null if TRUE, allow the path to be NULL.
#'                   If FALSE (default) do not allow NULL.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::file_exists(f))
#' # For check
#' if (qscheck::file_exists(f)) {}
#' }
#'
#'
#' @concept filesystem
#' @export
is_existing_file <- function(path, allow_null = FALSE) {
  res <- inspect_is_existing_file(path, allow_null)
  return(res$valid)
}

assertthat::on_failure(is_existing_file) <- function(call, env) {
  path <- callget(call, env, "path", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  res <- inspect_is_existing_file(path, allow_null)

  msg <- paste0(
    deparse(call$path),
    snippet_must_be("path to an existing file"),
    snippet_null(allow_null),
    ". ", res$reason
  )

  return(msg)
}

inspect_is_existing_file <- function(path, allow_null) {
  res <- inspect_string_value(path, allow_null = allow_null)

  if (!res$valid) {
    return(res)
  }

  if (is.null(path)) {
    return(success())
  }

  if (!(file.exists(path))) {
    return(failure(
      paste0("Path '", path, "' does not refer to an existing file")))
  }

  # The fact that the file exists does not mean it's an actual file.
  # It might be a directory, because R has no consistency of terms.
  # file.exists should have been called path.exists. So we have to check
  # if it's a directory and exclude that case.

  if (dir.exists(path)) {
    return(failure(paste0("Path '", path, "' is a directory")))
  }

  return(success())

}

#' Checks if the path refers to an existing directory.
#'
#' @param path the value to check
#' @param allow_null if TRUE, allow the value NULL. If FALSE (default) do not
#'                   allow NULL.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_existing_dir(f))
#' # For check
#' if (qscheck::is_existing_dir(f)) {}
#' }
#'
#'
#' @concept filesystem
#' @export
is_existing_dir <- function(path, allow_null = FALSE) {
  res <- inspect_is_existing_dir(path, allow_null)
  return(res$valid)
}

assertthat::on_failure(is_existing_dir) <- function(call, env) {
  path <- callget(call, env, "path", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  res <- inspect_is_existing_dir(path, allow_null)

  msg <- paste0(
    deparse(call$path),
    snippet_must_be("path to an existing directory"),
    snippet_null(allow_null),
    ". ", res$reason
  )

  return(msg)
}

inspect_is_existing_dir <- function(path, allow_null) {
  res <- inspect_string_value(path, allow_null = allow_null)

  if (!res$valid) {
    return(res)
  }

  if (is.null(path)) {
    return(success())
  }

  # Help the user by differentiating the error in case the directory does not
  # exist vs the path is referring to a file instead.
  # file.exists returns true also for directories (although the documentation
  # is very undecided on that)

  if (!(file.exists(path))) {
    return(failure(
      paste0("Path '", path, "' does not refer to an existing directory")
      )
    )
  }

  # If we arrive here, something exist, and it's either a file or a directory.
  # Check if it's actually a directory
  if (!(dir.exists(path))) {
    return(failure(
      paste0("Path '", path, "' refers to an existing file instead")
      )
    )
  }

  return(success())

}
