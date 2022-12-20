#' Checks if the passed entity is an S3 instance.
#'
#' @param value the value to check
#' @param class_name the name of the class, as a string.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_s3_instance(value, "MyClass")
#'   )
#' # For check
#' if (qscheck::is_s3_instance(value, "MyClass")) {}
#' }
#'
#' @concept oop
#' @export
is_s3_instance <- function(value, class_name, allow_null = FALSE) {
  res <- inspect_s3_instance(value, class_name, allow_null = allow_null)
  return(res$valid)
}
assertthat::on_failure(is_s3_instance) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  class_name <- callget(call, env, "class_name", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)
  res <- inspect_s3_instance(value, class_name, allow_null = allow_null)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be(paste0("instance of S3 class ", call$class_name)),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}
inspect_s3_instance <- function(
  value, class_name, allow_null = FALSE
) {
  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value cannot be NULL"))
    }
  }

  if (is.null(class_name)) {
    return(failure("Passed class name cannot be NULL"))
  }

  if (!inherits(value, class_name)) {
    return(failure(
      "Passed value is not an instance of the specifieed S3 class"))
  }

  return(success())

}
