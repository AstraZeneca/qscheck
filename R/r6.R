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
  res <- inspect_r6_class(value, class_name)

  return(res$valid)
}
assertthat::on_failure(is_r6_class) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  class_name <- callget(call, env, "class_name", NULL)

  res <- inspect_r6_class(value, class_name)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be(paste0("R6 class '", class_name, "'")),
    ". ", res$reason
  )
  return(msg)
}
inspect_r6_class <- function(value, class_name) {
  if (is.null(value)) {
    return(failure("Passed class cannot be NULL"))
  }

  if (!inherits(value, "R6ClassGenerator")) {
    return(failure("Passed value is not an R6 class"))
  }

  if (value$classname != class_name) {
    return(failure(
      paste0("Passed value is an R6 class '", value$classname, "'")))
  }

  return(success())

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
  res <- inspect_r6_instance(value, class_name, allow_null)

  return(res$valid)
}
assertthat::on_failure(is_r6_instance) <- function(call, env) {
  value <- callget(call, env, "value", NULL)
  class_name <- callget(call, env, "class_name", NULL)
  allow_null <- callget(call, env, "allow_null", FALSE)

  res <- inspect_r6_instance(value, class_name, allow_null)

  msg <- paste0(
    deparse(call$value),
    snippet_must_be(paste0("instance of R6 class '", class_name, "'")),
    snippet_null(allow_null),
    ". ", res$reason
  )
  return(msg)
}
inspect_r6_instance <- function(value, class_name, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null == TRUE) {
      return(success())
    } else {
      return(failure("Passed value is NULL"))
    }
  }

  if (!inherits(value, "R6")) {
    return(failure("Passed value is not an R6 instance"))
  }

  if (!inherits(value, class_name)) {
    return(failure(
      paste0("Passed value is not an instance of class '", class_name, "'")
      )
    )
  }

  return(success())

}
