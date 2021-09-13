#' Checks if the passed value is a single integer value (not type)
#'
#' @param value the value to verify
#' @param min minimum value to constraint the integer, inclusive
#' @param max maximum value to constraint the integer, inclusive
#' @param allow_null if TRUE, accepts a null value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_integer_value(my_parameter))
#' # For check
#' if (qscheck::is_integer_value(my_parameter)) {}
#' }
#'
#' @export
is_integer_value <- function(value,
    min = NULL, max = NULL, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  if ((value %% 1) != 0) {
    return(FALSE)
  }

  if (!is.null(min) && value < min) {
    return(FALSE)
  }

  if (!is.null(max) && value > max) {
    return(FALSE)
  }

  return(TRUE)

}
assertthat::on_failure(is_integer_value) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  interval_msg <- ""
  if (!is.null(call$min) || !is.null(call$max)) {
    interval_msg <- " in the range "
    if (is.null(call$min)) {
      interval_msg <- paste0(interval_msg, "(-inf, ")
    } else {
      min <- (eval(call$min, env))
      interval_msg <- paste0(interval_msg, "[", deparse(min), ", ")
    }

    if (is.null(call$max)) {
      interval_msg <- paste0(interval_msg, "inf)")
    } else {
      max <- (eval(call$max, env))
      interval_msg <- paste0(interval_msg, deparse(max), "]")
    }
  }

  return(paste0(deparse(call$value),
                " must be an integer value",
                interval_msg,
                allow_null_msg,
                ". Got: ",
                deparse(eval(call$value, env))))
}

#' Checks if the value is a single positive integer value (not type)
#'
#' @param value the value to verify
#' @param allow_null accepts a null value
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_positive_integer_value(my_parameter))
#' # For check
#' if (qscheck::is_positive_integer_value(my_parameter)) {}
#' }
#'
#' @export
is_positive_integer_value <- function(value, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }
  return(is_integer_value(value) && (value > 0))
}
assertthat::on_failure(is_positive_integer_value) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }
  return(
    paste0(
      deparse(call$value),
      " must be a positive integer value",
      allow_null_msg,
      ". Got: ",
      deparse(eval(call$value, env))
    )
  )
}

#' Checks if the passed value is a single string.
#'
#' @param value the value to verify
#' @param allow_empty If TRUE (default) accept an empty string. If FALSE, don't
#' @param allow_na If TRUE, accepts a single NA value. If FALSE (default) don't
#' @param allow_null If TRUE, accepts a single NULL value.
#'                   If FALSE (default) don't
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_string_value(my_parameter))
#' # For check
#' if (qscheck::is_string_value(my_parameter)) {}
#' }
#'
#' @export
is_string_value <- function(value, allow_empty = TRUE,
                      allow_na = FALSE, allow_null = FALSE) {
  if (is_na_value(value) && allow_na) {
    return(TRUE)
  }

  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!assertthat::is.string(value)) {
    return(FALSE)
  }

  if (nchar(value) == 0) {
    if (allow_empty) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_string_value) <- function(call, env) {
  non_empty_msg <- ""

  if (!is.null(call$allow_empty)) {
    if (!eval(call$allow_empty, env)) {
      non_empty_msg <- " non-empty"
    }
  }

  allow_na_msg <- ""
  if (!is.null(call$allow_na)) {
    if (eval(call$allow_na, env)) {
      allow_na_msg <- " or NA"
    }
  }

  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  msg <- paste0(
    deparse(call$value),
    " must be a",
    non_empty_msg,
    " string",
    allow_na_msg,
    allow_null_msg,
    ". Got: ",
    deparse(eval(call$value, env))
    )
  return(msg)
}

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
        required_colnames_msg), collapse = " and "
      ),
    allow_na_msg,
    allow_null_msg,
    ". Got: ",
    paste0(deparse(eval(call$df, env)), collapse = ""))
  return(msg)
}

#' Check if the passed entity is a list.
#'
#' @param l the value to check
#' @param required_names vector of strings. If passed, checks if the list
#'                   contains *at least* the specified elements.
#' @param exact_length integer value. If passed, the list must have the
#'                    *exact* specified length
#' @param allow_null if TRUE, NULL is accepted as a valid value.
#'                   If FALSE (default) do not accept it.
#' @details
#' Note that a list with names set to NULL are considered not to be present.
#' This is an R detail, not a qscheck detail.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_list(my_parameter))
#' # For check
#' if (qscheck::is_list(my_parameter)) {}
#' }
#'
#'
#' @export
is_list <- function(
    l,
    required_names = NULL,
    exact_length = NULL,
    allow_null = FALSE
    ) {

  if (is.null(l) && allow_null) {
    return(TRUE)
  }

  if (!inherits(l, "list")) {
    return(FALSE)
  }
  if (!is.null(required_names)) {
    for (name in required_names) {
      if (!(name %in% names(l))) {
        return(FALSE)
      }
    }
  }

  if (!is.null(exact_length)) {
    return(assertthat::are_equal(length(l), exact_length))
  }
  return(TRUE)
}
assertthat::on_failure(is_list) <- function(call, env) {
  msg <- paste0(deparse(call$l), " must be a list")
  if (!is.null(call$exact_length)) {
    msg <- paste0(msg, " of exact length ", eval(call$exact_length, env),
                  " (passed ", length(eval(call$l, env)), ")")
  }

  if (!is.null(call$required_names)) {
    msg <- paste0(msg, " with at least names '",
                  paste0(
                    eval(call$required_names, env),
                    collapse = "', '"
                    ),
                  "'"
                  )
  }
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      msg <- paste0(msg, " or NULL")
    }
  }
  msg <- paste0(
   msg,
   ". Got: ",
   deparse(eval(call$l, env))
  )
  return(msg)
}

#' Checks if the passed entity is a vector. Note that in R single values
#' are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                    *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_vector(my_parameter))
#' # For check
#' if (qscheck::is_vector(my_parameter)) {}
#' }
#'
#' @export
is_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL) {
  if (!is.vector(value)) {
    return(FALSE)
  }
  if (!is.null(exact_length)) {
    return(assertthat::are_equal(length(value), exact_length))
  }

  if (!is.null(min_length)) {
    if (length(value) < min_length) {
      return(FALSE)
    }
  }

  if (!is.null(max_length)) {
    if (length(value) > max_length) {
      return(FALSE)
    }
  }
  return(TRUE)
}
assertthat::on_failure(is_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a vector")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env),
      ". Got: ",
      deparse(eval(call$value, env))

    )
    return(msg)
  }

  if (!is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      eval(call$min_length, env),
      " and ",
      eval(call$max_length, env),
      " inclusive"
    )
  } else if (is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      eval(call$max_length, env)
    )
  } else if (!is.null(call$min_length) && is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      eval(call$min_length, env)
    )
  }
  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if the passed entity is a vector of strings (character).
#' Note that in R single values are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                    *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_string_vector(my_parameter))
#' # For check
#' if (qscheck::is_string_vector(my_parameter)) {}
#' }
#'
#' @export
is_string_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE) {
  if (!is_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)) {
    return(FALSE)
  }
  if (!is.character(value)) {
    return(FALSE)
  }
  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }
  return(TRUE)
}
assertthat::on_failure(is_string_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a string vector")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env),
      ". Got: ",
      deparse(eval(call$value, env))

    )
    return(msg)
  }

  if (!is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      eval(call$min_length, env),
      " and ",
      eval(call$max_length, env),
      " inclusive"
    )
  } else if (is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      eval(call$max_length, env)
    )
  } else if (!is.null(call$min_length) && is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      eval(call$min_length, env)
    )
  }
  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if two vectors are of the exact same length.
#'
#' @param v1 the first vector
#' @param v2 the first vector
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::vectors_same_length(v1, v2))
#' # For check
#' if (qscheck::vectors_same_length(v1, v2)) {}
#' }
#'
#' @export
vectors_same_length <- function(v1, v2) {
  if (! (is_vector(v1) && is_vector(v2))) {
      return(FALSE)
  }
  return(assertthat::are_equal(length(v1), length(v2)))
}
assertthat::on_failure(vectors_same_length) <- function(call, env) {
  return(paste0(
    "vectors ", deparse(call$v1), " and ", deparse(call$v2),
    " must have the exact same length."))
}

#' Check if the passed entity is a single logical value (TRUE or FALSE)
#'
#' @param value the value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_logical_value(value))
#' # For check
#' if (qscheck::is_logical_value(value)) {}
#' }
#'
#' @export
is_logical_value <- function(value) {
  return(is.logical(value) && length(value) == 1)
}
assertthat::on_failure(is_logical_value) <- function(call, env) {
  return(paste0(
    deparse(call$value),
    " must be a single logical value. Got: ",
    deparse(eval(call$value, env))
  ))
}

#' Check if the passed entity is a single floating point positive value.
#'
#' @param value the value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_positive_real_value(value))
#' # For check
#' if (qscheck::is_positive_real_value(value)) {}
#' }
#'
#' @export
is_positive_real_value <- function(value) {
  return(is_real_value(value, min = 0.0, inclusive_min = FALSE))
}
assertthat::on_failure(is_positive_real_value) <- function(call, env) {
  return(paste0(deparse(call$value),
                 " must be a positive real value. Got: ",
                 deparse(eval(call$value, env))))
}

#' Checks if the passed entity is one of the ones in the options vector.
#'
#' @param value the value to check
#' @param options a vector of options.
#'        Value must be one of these for the check not to fail
#' @param allow_null if TRUE, allow the value NULL. If FALSE (default) do not
#'                   allow NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_one_of(value, c("foo", "bar")))
#' # For check
#' if (qscheck::is_one_of(value, c("foo", "bar"))) {}
#' }
#'
#' @export
is_one_of <- function(value, options, allow_null = NULL) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (typeof(value) != typeof(options)) {
    return(FALSE)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  return(value %in% options)
}
assertthat::on_failure(is_one_of) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }
  msg <- paste0(deparse(call$value), " must be one of the following: '",
                paste0(
                  eval(call$options, env),
                  collapse = "', '"
                  ),
                "'",
                allow_null_msg,
                ". Got: ",
                deparse(eval(call$value, env))
                )
  return(msg)
}

#' Checks if the value is a function
#'
#' @param value the value to check
#' @param args a vector of strings. If defined, the function must have
#'             the exact arguments, in the specified order.
#' @param allow_null if TRUE, allow the value NULL. If FALSE (default) do not
#'                   allow NULL.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_function(f))
#' # For check
#' if (qscheck::is_function(f)) {}
#' }
#'
#'
#' @export
is_function <- function(value, args = NULL, allow_null = FALSE) {
  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!inherits(value, "function")) {
    return(FALSE)
  }

  if (!is.null(args)) {
    len <- length(args)
    func_args <- formals(value)

    if (length(args) != length(func_args)) {
      return(FALSE)
    }

    for (idx in seq_len(len)) {
      if (args[idx] != names(func_args)[idx]) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_function) <- function(call, env) {
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  args_msg <- ""
  if (!is.null(call$args)) {
    args_msg <- paste0(
      " with arguments ",
      paste(
        eval(call$args, env),
        collapse = ", "
      )
    )
  }

  msg <- paste0(
    deparse(call$value),
    " must be a function",
    args_msg,
    allow_null_msg,
    ". Got: ",
    deparse(call$value))
  return(msg)
}

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
#'
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
  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  msg <- paste0(
    deparse(call$value),
    " must be an instance of R6 class ",
    call$class_name,
    allow_null_msg,
    ". Got: ",
    deparse(eval(call$value, env)))
  return(msg)
}

#' Checks if any one of a set of assertions is satisfied
#'
#' @description
#' Allows to check for multiple cases, and pass the test if any of the cases
#' is satisfied.
#'
#' @param ... the various assertions that can be satisfied
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::any_satisfied(
#'    qscheck::is_positive_integer_value(value),
#'    qscheck::is_list(value)
#'   )
#' )
#' # For check
#' if (
#'   qscheck::any_satisfied(
#'    qscheck::is_positive_integer_value(value),
#'    qscheck::is_list(value)
#'   )
#' ) {}
#' }
#'
#' @export
any_satisfied <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    return(FALSE)
  }

  return(any(as.logical(dots)))
}
assertthat::on_failure(any_satisfied) <- function(call, env) {
  msg <- paste0(
    deparse(call),
    ": none one of possible options were satisfied.")
  return(msg)
}


#' Checks if a value is a single NA of any type.
#'
#' @description
#' Allows to check if a given value is a single NA value, regardless
#' of its NA type.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' assertthat::assert_that(
#'   qscheck::is_na_value(value)
#' )
#' }
#'
#' @export
is_na_value <- function(value) {
  return(is.vector(value) && length(value) == 1 && is.na(value))
}
assertthat::on_failure(is_na_value) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be NA",
    ". Got: ",
    deparse(eval(call$value, env)))
  return(msg)
}

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

#' Check if the passed entity is a single floating point value.
#' Optionally, specify the minimum and maximum values allowed, either
#' inclusive or exclusive.
#'
#' @param value the value to check
#' @param min the minimum allowed value, inclusive or exclusive.
#' @param max the maximum allowed value, inclusive or exclusive.
#' @param inclusive_min if TRUE (default) the min value is checked inclusive.
#'        If FALSE, the min value will be checked exclusive.
#' @param inclusive_max if TRUE (default) the max value is checked inclusive.
#'        If FALSE, the max value will be checked exclusive
#' @param allow_na if true, accept a value that is NA.
#' @param allow_null if true, accept a value that is NULL.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_real_value(value))
#' # For check
#' if (qscheck::is_real_value(value)) {}
#' }
#'
#' @export
is_real_value <- function(value,
    min = NULL, max = NULL,
    inclusive_min = TRUE, inclusive_max = TRUE,
    allow_na = FALSE, allow_null = FALSE) {

  if (is.null(value)) {
    return(ifelse(allow_null, TRUE, FALSE))
  }

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (length(value) != 1) {
    return(FALSE)
  }

  if (is_na_value(value)) {
    return(ifelse(allow_na, TRUE, FALSE))
  }

  if (!is.null(min)) {
    if (inclusive_min && value < min) {
      return(FALSE)
    } else if (!inclusive_min && value <= min) {
      return(FALSE)
    }
  }

  if (!is.null(max)) {
    if (inclusive_max && value > max) {
      return(FALSE)
    } else if (!inclusive_max && value >= max) {
      return(FALSE)
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_real_value) <- function(call, env) {
  allow_na_msg <- ""
  if (!is.null(call$allow_na)) {
    if (eval(call$allow_na, env)) {
      allow_na_msg <- " or NA"
    }
  }

  allow_null_msg <- ""
  if (!is.null(call$allow_null)) {
    if (eval(call$allow_null, env)) {
      allow_null_msg <- " or NULL"
    }
  }

  interval_msg <- ""
  if (!is.null(call$min) || !is.null(call$max)) {
    interval_msg <- " in the range "
    if (is.null(call$min)) {
      interval_msg <- paste0(interval_msg, "(-inf, ")
    } else {
      min <- (eval(call$min, env))
      inclusive_min <- (eval(call$inclusive_min, env))
      interval_msg <- paste0(
        interval_msg,
        ifelse(inclusive_min, "[", "("),
        deparse(min), ", ")
    }

    if (is.null(call$max)) {
      interval_msg <- paste0(interval_msg, "inf)")
    } else {
      max <- (eval(call$max, env))
      inclusive_max <- (eval(call$inclusive_max, env))
      interval_msg <- paste0(
        interval_msg, deparse(max),
        ifelse(inclusive_max, "]", ")"))
    }
  }

  return(paste0(deparse(call$value),
                " must be a single real value",
                interval_msg,
                allow_na_msg,
                allow_null_msg,
                ". Got: ",
                deparse(eval(call$value, env))))
}


#' Check if the passed entity is a single probability value, that is, a
#' floating point value in the interval [0.0, 1.0].
#'
#' @param value the value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_probability_value(value))
#' # For check
#' if (qscheck::is_probability_value(value)) {}
#' }
#'
#' @export
is_probability_value <- function(value) {
  return(is_real_value(value, min = 0.0, max = 1.0))
}
assertthat::on_failure(is_probability_value) <- function(call, env) {
  return(paste0(deparse(call$value),
                 " must be a single probability value in ",
                 "the interval [0.0, 1.0]. Got: ",
                 deparse(eval(call$value, env))))
}

#' Checks if the passed values are mutually exclusive.
#'
#' @description
#' Checks if the passed values are mutually exclusive, that is, if
#' one of the passed values has a non-NULL value, all the others
#' _must_ be NULL.
#' If all options are NULL, this test passes.
#' Allows to check for multiple cases, and pass the test if any of the cases
#' is satisfied.
#' This test is generally useful if two or more parameters in a routine are
#' technically incompatible with each other, and you want the user to know
#' the passed parameters in that combination don't make any sense.
#'
#' @param ... the various values
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::mutually_exclusive(parameter1, parameter2, parameter3)
#' )
#' # For check
#' if (
#'   qscheck::mutually_exclusive(parameter1, parameter2, parameter3)
#' ) {}
#' }
#'
#' @export
mutually_exclusive <- function(...) {
  dots <- list(...)

  # Prevent empty call
  if (length(dots) == 0) {
    return(FALSE)
  }

  not_null <- dots[!sapply(dots, is.null)]
  return(length(not_null) < 2)
}
assertthat::on_failure(mutually_exclusive) <- function(call, env) {
  args <- as.list(call)

  not_nulls <- list()
  for (i in seq_along(args)) {
    if (i == 1) {
      next
    }
    arg <- args[[i]]
    not_nulls[[arg]] <- eval(call[[i]], env)
  }

  msg <- paste0(
    "'",
    paste0(
      names(not_nulls),
      collapse = "', '"
      ),
    "' must be mutually exclusive. Got ",
    paste0(not_nulls, collapse = ", ")
    )
  return(msg)
}

#' Checks if two vectors are disjoint.
#'
#' @description
#' Allows to check if two vectors are disjoint. Two vectors
#' are disjoint if they are of the same type and none of the elements
#' present in one can be found in the other.
#'
#' @param v1 the first vector
#' @param v2 the second vector
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::vectors_disjoint(v1, v2)
#'   )
#' )
#' # For check
#' if (
#'   qscheck::vectors_disjoint(v1, v2)
#'   )
#' ) {}
#' }
#'
#' @export
vectors_disjoint <- function(v1, v2) {
  if (!(is_vector(v1) && is_vector(v2))) {
    return(FALSE)
  }

  if (typeof(v1) != typeof(v2)) {
    return(TRUE)
  }

  return(length(intersect(v1, v2)) == 0)
}
assertthat::on_failure(vectors_disjoint) <- function(call, env) {
  v1 <- eval(call$v1, env)
  v2 <- eval(call$v2, env)
  if (!is_vector(v1)) {
    return(paste0(deparse(call$v1), " must be a vector. Got: ", class(v1)))
  }

  if (!is_vector(v2)) {
    return(paste0(deparse(call$v2), " must be a vector. Got: ", class(v2)))
  }

  common <- intersect(v1, v2)
  return(paste0(
    deparse(call$v1), " and ", deparse(call$v2),
    " must be disjoint vectors but the following elements",
    " are present in both: ", paste0(common, collapse = ", "))
  )
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


#' Checks if a vector contains values only out of a set of available options.
#'
#' @param v the vector
#' @param allowed_values a vector of allowed values
#'
#' @examples
#' \dontrun{
#' # For assertion.
#' # Will fail if v contains any value that is not either 1, 2 or 3
#' assertthat::assert_that(qscheck::vector_allowed_values(v, c(1, 2, 3)))
#' # For check
#' if (qscheck::vector_allowed_values(v, c(1, 2, 3)) {}
#' }
#'
#' @export
vector_allowed_values <- function(v, allowed_values) {
  if (!is_vector(v) || !is_vector(allowed_values)) {
    return(FALSE)
  }

  if (typeof(v) != typeof(allowed_values)) {
    return(FALSE)
  }

  return(all(v %in% allowed_values))
}
assertthat::on_failure(vector_allowed_values) <- function(call, env) {
  return(paste0(
    deparse(call$v),
    " must be a vector containing only elements from the following list: ",
    paste0(
      eval(call$allowed_values, env),
      collapse = ", "
    )
  ))
}

#' Checks if a vector contains only binary values (0 or 1)
#'
#' @param v the vector
#' @param allow_na_values if the vector can contain NA values. Default FALSE
#'
#' @examples
#' \dontrun{
#' # For assertion.
#' # Will fail if v contains any value that is not either 0 or 1
#' assertthat::assert_that(qscheck::is_binary_vector(v))
#' # For check
#' if (qscheck::is_binary_vector(v)) {}
#' }
#'
#' @export
is_binary_vector <- function(v, allow_na_values = FALSE) {
  if (allow_na_values == TRUE) {
    allowed <- c(0, 1, NA)
  } else {
    allowed <- c(0, 1)
  }

  if (!vector_allowed_values(v, allowed)) {
    return(FALSE)
  }
  return(TRUE)
}
assertthat::on_failure(is_binary_vector) <- function(call, env) {
  allow_na_values_msg <- ""
  if (!is.null(call$allow_na_values)) {
    allow_na_values_msg <- " or NA"
  }

  return(
    paste0(
      deparse(call$v),
      " must be a vector of binary values (0 or 1",
      allow_na_values_msg, ")"
    )
  )
}

#' Checks if the passed entity is a vector of reals (numeric).
#' Note that in R single values are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                    *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_real_vector(my_parameter))
#' # For check
#' if (qscheck::is_real_vector(my_parameter)) {}
#' }
#'
#' @export
is_real_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE) {
  if (!is_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)) {
    return(FALSE)
  }
  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_real_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a vector of real numbers")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env),
      ". Got: ",
      deparse(eval(call$value, env))

    )
    return(msg)
  }

  if (!is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      eval(call$min_length, env),
      " and ",
      eval(call$max_length, env),
      " inclusive"
    )
  } else if (is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      eval(call$max_length, env)
    )
  } else if (!is.null(call$min_length) && is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      eval(call$min_length, env)
    )
  }
  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if the passed entity is a vector of non negative integers.
#' Note that in R single values are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                    *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_non_negative_integer_vector(my_parameter)
#' )
#' # For check
#' if (qscheck::is_non_negative_integer_vector(my_parameter)) {}
#' }
#'
#' @export
is_non_negative_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

  if (!is_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)) {
    return(FALSE)
  }

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  value <- value[!is.na(value)]

  if (!(all(value %% 1 == 0) && all(value >= 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_non_negative_integer_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value),
    " must be a vector of non negative integer numbers")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env),
      ". Got: ",
      deparse(eval(call$value, env))

    )
    return(msg)
  }

  if (!is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      eval(call$min_length, env),
      " and ",
      eval(call$max_length, env),
      " inclusive"
    )
  } else if (is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      eval(call$max_length, env)
    )
  } else if (!is.null(call$min_length) && is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      eval(call$min_length, env)
    )
  }
  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if the passed entity is a vector of integers. NOTE:  NOT of integer
#' type. Numerics that are whole numbers.
#' Note that in R single values are also vectors.
#'
#' @param value The value to check
#' @param exact_length integer. If passed, checks if the vector is of the
#'                    *exact* specified length.
#' @param min_length integer. If passed, checks if the vector length
#'                   is at least the specified length, inclusive.
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param max_length integer. If passed, checks if the vector length
#'                   is at most the specified length, inclusive
#'                   Note: if exact_length is specified, this parameter is
#'                   ignored
#' @param allow_na_values boolean: If passed allows vectors containing
#'                        NAs. The length check is performed including
#'                        the NA values. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_integer_vector(my_parameter)
#' )
#' # For check
#' if (qscheck::is_integer_vector(my_parameter)) {}
#' }
#'
#' @export
is_integer_vector <- function(
    value, exact_length = NULL, min_length = NULL, max_length = NULL,
    allow_na_values = FALSE
    ) {

  if (!is_vector(
      value,
      exact_length = exact_length,
      min_length = min_length,
      max_length = max_length)) {
    return(FALSE)
  }

  if (!is.numeric(value)) {
    return(FALSE)
  }

  if (any(is.na(value)) && allow_na_values == FALSE) {
    return(FALSE)
  }

  value <- value[!is.na(value)]

  if (!(all(value %% 1 == 0))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_integer_vector) <- function(call, env) {
  msg <- paste0(deparse(call$value),
    " must be a vector of integer values")

  if (!is.null(call$exact_length)) {
    msg <- paste0(
      msg,
      " of exact length ", eval(call$exact_length, env),
      ". Got: ",
      deparse(eval(call$value, env))

    )
    return(msg)
  }

  if (!is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length between ",
      eval(call$min_length, env),
      " and ",
      eval(call$max_length, env),
      " inclusive"
    )
  } else if (is.null(call$min_length) && !is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not greater than ",
      eval(call$max_length, env)
    )
  } else if (!is.null(call$min_length) && is.null(call$max_length)) {
    msg <- paste0(
      msg,
      " of length not less than ",
      eval(call$min_length, env)
    )
  }
  msg <- paste0(
    msg,
    ". Got: ",
    deparse(eval(call$value, env))
  )
  return(msg)
}

#' Checks if the passed vector contains no NAs.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_vector_without_na(my_parameter)
#' )
#' # For check
#' if (qscheck::is_vector_without_na(my_parameter)) {}
#' }
#'
#' @export
is_vector_without_na <- function(value) {
  if (!is_vector(value)) {
    return(FALSE)
  }

  if (any(is.na(value))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_vector_without_na) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a vector with no NA values.")

  return(msg)
}

#' Checks if the passed vector contains only NAs.
#'
#' @param value The value to check
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::is_vector_all_na(my_parameter)
#' )
#' # For check
#' if (qscheck::is_vector_all_na(my_parameter)) {}
#' }
#'
#' @export
is_vector_all_na <- function(value) {
  if (!is_vector(value)) {
    return(FALSE)
  }

  if (!all(is.na(value))) {
    return(FALSE)
  }

  return(TRUE)
}
assertthat::on_failure(is_vector_all_na) <- function(call, env) {
  msg <- paste0(
    deparse(call$value),
    " must be a vector containing only NA values."
  )

  return(msg)
}

#' Checks if a given value occurs a given number of times in a vector
#'
#' @param vec The vector to check
#' @param value The value to check
#' @param exact_occurrences the exact number of times the value must be
#'        present. Takes precedence over min/max_occurrences.
#' @param min_occurrences the minimum number of times (inclusive) the
#'                        value must be present.
#' @param max_occurrences the maximum number of times (inclusive) the
#'                        value must be present.
#'
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(
#'   qscheck::vector_value_occurrences(
#'     vec, value, min_occurrences = 2, max_occurrences = 3
#'   )
#' )
#' # For check
#' if (
#'   qscheck::vector_value_occurrences(
#'     vec, value, min_occurrences = 2, max_occurrences = 3
#'   )
#' }
#'
#' @export
vector_value_occurrences <- function(
      vec,
      value,
      exact_occurrences = NULL,
      min_occurrences = NULL,
      max_occurrences = NULL
      ) {

  if (!is_vector(vec)) {
    return(FALSE)
  }

  total_occurrences <- sum(vec == value, na.rm = TRUE)

  if (!is.null(exact_occurrences)) {
    return(total_occurrences == exact_occurrences)
  }

  if (!is.null(min_occurrences)) {
    if (total_occurrences < min_occurrences) {
      return(FALSE)
    }
  }

  if (!is.null(max_occurrences)) {
    if (total_occurrences > max_occurrences) {
      return(FALSE)
    }
  }

  return(TRUE)

}
assertthat::on_failure(vector_value_occurrences) <- function(call, env) {
  msg <- paste0(
    deparse(call$vec),
    " must be a vector containing value '",
    eval(call$value, env),
    "'"
  )

  total_occurrences <- sum(
    eval(call$vec, env) == eval(call$value, env), na.rm = TRUE
  )

  if (!is.null(call$exact_occurrences)) {
    msg <- paste0(
      msg,
      " exactly ", eval(call$exact_occurrences, env),
      " times. Found it ",
      total_occurrences,
      " times."
    )
    return(msg)
  }

  if (!is.null(call$min_occurrences) && !is.null(call$max_occurrences)) {
    msg <- paste0(
      msg,
      " between ",
      eval(call$min_occurrences, env),
      " and ",
      eval(call$max_occurrences, env),
      " times inclusive."
    )
  } else if (is.null(call$min_occurrences) && !is.null(call$max_occurrences)) {
    msg <- paste0(
      msg,
      " no more than ",
      eval(call$max_occurrences, env),
      " times inclusive."
    )
  } else if (!is.null(call$min_occurrences) && is.null(call$max_occurrences)) {
    msg <- paste0(
      msg,
      " no less than ",
      eval(call$min_occurrences, env),
      " times inclusive."
    )
  }
  msg <- paste0(
    msg,
    " Found it ",
    total_occurrences,
    " times."
  )
  return(msg)
}


#' Check if the passed entity is a factor.
#'
#' @param value the value to check
#' @param exact_levels vector of strings.
#'                   contains *eat least* the specified elements.
#' @param exact_length integer value. If passed, the factor must have the
#'                    *exact* specified length
#' @param allow_null if TRUE, NULL is accepted as a valid value.
#'                   If FALSE (default) do not accept it.
#' @examples
#' \dontrun{
#' # For assertion
#' assertthat::assert_that(qscheck::is_factor(my_parameter))
#' # For check
#' if (qscheck::is_factor(my_parameter)) {}
#' }
#'
#'
#' @export
is_factor <- function(
  value, exact_levels = NULL, exact_length = NULL, allow_null = FALSE) {

  if (is.null(value) && allow_null) {
    return(TRUE)
  }

  if (!is.factor(value)) {
    return(FALSE)
  }
  if (!is.null(exact_length)) {
    return(assertthat::are_equal(length(value), exact_length))
  }
  if (!is.null(exact_levels)) {
    if (!all(exact_levels == levels(value))) {
      return(FALSE)
    }
  }

  return(TRUE)
}
assertthat::on_failure(is_factor) <- function(call, env) {
  msg <- paste0(deparse(call$value), " must be a factor")
  if (!is.null(call$exact_length)) {
    msg <- paste0(msg, " of exact length ", eval(call$exact_length, env))
  }

  if (!is.null(call$exact_levels)) {
    msg <- paste0(msg, " with exact levels '",
                  paste0(
                    eval(call$exact_levels, env),
                    collapse = "', '"
                    ),
                  "'"
                  )
  }
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
