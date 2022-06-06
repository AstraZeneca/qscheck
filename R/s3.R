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
#' @export
is_s3_instance <- function(value, class_name, allow_null=FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (is.null(class_name)) {
    return(FALSE)
  }

  return(inherits(value, class_name))
}
assertthat::on_failure(is_s3_instance) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be an instance of S3 class ",
    call$class_name
    )

  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      msg <- paste0(msg, " or NULL")
    }
  }

  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}
