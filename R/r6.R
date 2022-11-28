#' Checks if the passed entity is a R6 class.
#'
#' @param value the value to check
#' @param class_name The name of the class
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_r6_class(value, "MyClass"))
#' # For check
#' if (qscheck::is_r6_class(value, "MyClass")) {}
#' }
#'
#' @concept oop
#' @export
is_r6_class <- function(value, class_name) {
  if (is.null(class_name) || is.null(value)) {
    return(FALSE)
  }

  return(length(class(value)) >= 1
         && "R6ClassGenerator" %in% class(value)
         && value$classname == class_name)
}
assertthat::on_failure(is_r6_class) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be an R6 class ",
    call$class_name,
    ". Got: ",
    deparse(eval(call$value, env)))
  return(msg)
}

#' Checks if the passed entity is an instance of a given R6 class name.
#'
#' @param value the value to check
#' @param class_name The name of the class
#' @param allow_null If TRUE, allow the value to be NULL. If FALSE (default)
#'                   do not accept NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_r6_instance(value, "MyClass"))
#' # For check
#' if (qscheck::is_r6_instance(value, "MyClass")) {}
#' }
#'
#' @concept oop
#' @export
is_r6_instance <- function(value, class_name, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (is.null(class_name)) {
    return(FALSE)
  }

  return(length(class(value)) > 1
         && class_name %in% class(value)
         && class(value)[[length(class(value))]] == "R6")
}
assertthat::on_failure(is_r6_instance) <- function(call, env) {
  allow_null <- callget(call, env, "allow_null", FALSE)

  msg <- paste0(
    deparse(call$value),
    " must be an instance of R6 class ",
    call$class_name,
    snippet_null(allow_null),
    ". Got: ",
    deparse(eval(call$value, env)))
  return(msg)
}
