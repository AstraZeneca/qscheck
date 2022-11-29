#' Checks if the passed entity is an S4 instance of a specified class.
#'
#' @param value the value to check
#' @param class_name the name of the class, as a string.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_s4_instance(value, "MyClass")
#'   )
#' # For check
#' if (qscheck::is_s4_instance(value, "MyClass")) {}
#' }
#'
#' @concept oop
#' @export
is_s4_instance <- function(value, class_name, allow_null = FALSE) {

  if (is.null(value)) {
    return(allow_null)
  }

  if (is.null(class_name)) {
    return(FALSE)
  }

  if (!(isS4(value))) {
    return(FALSE)
  }

  if (!(is(value, class_name))) {
    return(FALSE)
  }

  return(TRUE)

}

assertthat::on_failure(is_s4_instance) <- function(call, env) {
  allow_null <- callget(call, env, "allow_null", FALSE)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be(paste0("instance of S4 class ", call$class_name)),
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}
