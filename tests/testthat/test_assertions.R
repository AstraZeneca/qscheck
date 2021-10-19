context("assertions")

test_that("is_string_value", {
  expect_true(is_string_value("ada"))
  expect_true(is_string_value(""))
  expect_false(is_string_value("", allow_empty = FALSE))
  expect_true(is_string_value(NA, allow_na = TRUE))
  expect_false(is_string_value(2))
  expect_false(is_string_value(NA, allow_na = FALSE))
  expect_false(is_string_value(NA))
  expect_false(is_string_value(c("foo", "bar")))
})

test_that("is_positive_integer_value", {
  expect_true(is_positive_integer_value(3))
  expect_true(is_positive_integer_value(3.0))
  expect_false(is_positive_integer_value(3.5))
  expect_false(is_positive_integer_value(c(3, 4)))
  expect_false(is_positive_integer_value(-3))
  expect_false(is_positive_integer_value("hello"))
  expect_false(is_positive_integer_value(NULL))
  expect_true(is_positive_integer_value(NULL, allow_null = TRUE))
})

test_that("is_vector", {
  v <- c(1, 2, 3)
  expect_true(is_vector(v))
  expect_false(is_vector(NULL))
  expect_false(is_vector(v, exact_length = 4))
  expect_false(is_vector(v, min_length = 4))
  expect_true(is_vector(v, max_length = 4))
  expect_false(is_vector(v, max_length = 2))
  expect_true(is_vector(v, exact_length = 3, max_length = 2))

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    "Error: v must be a vector of exact length 4. Got: c(1, 2, 3)\n")

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    "Error: v must be a vector of length not less than 4. Got: c(1, 2, 3)\n")

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0("Error: v must be a vector of length not greater than 2. ",
    "Got: c(1, 2, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_vector(v, min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of length between 4 and 8 inclusive. ",
      "Got: c(1, 2, 3)\n"))
})

test_that("is_string_vector", {
  v <- c("foo", "bar", "baz")
  expect_true(is_string_vector(v))
  expect_false(is_string_vector(NULL))
  expect_false(is_string_vector(v, exact_length = 4))
  expect_false(is_vector(v, min_length = 4))
  expect_true(is_vector(v, max_length = 4))
  expect_false(is_vector(v, max_length = 2))
  expect_true(is_vector(v, exact_length = 3, max_length = 2))

  v2 <- "hello"
  expect_true(is_string_vector(v2))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0("Error: v must be a string vector of exact length 4. ",
    "Got: c(\"foo\", \"bar\", \"baz\")\n"))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0("Error: v must be a string vector of length not less than 4. ",
    "Got: c(\"foo\", \"bar\", \"baz\")\n"))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0("Error: v must be a string vector of length not greater than 2. ",
    "Got: c(\"foo\", \"bar\", \"baz\")\n"))

  err <- tryCatch({
    assertthat::assert_that(is_string_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a string vector of length between 4 and 8 inclusive. ",
      "Got: c(\"foo\", \"bar\", \"baz\")\n"))
})

test_that("vectors_disjoint", {
  v1 <- c("foo", "bar", "baz")
  v2 <- c(2, 3, 4)
  v3 <- c(2, "foo", 4)
  v4 <- c(5, 6, 7)
  v5 <- c(7, 8, 5)
  expect_true(vectors_disjoint(v1, v2))
  expect_false(vectors_disjoint(v1, v3))
  expect_true(vectors_disjoint(v2, v4))
  expect_true(vectors_disjoint(v2, v3))
  expect_false(vectors_disjoint(v4, v5))

  err <- tryCatch({
    assertthat::assert_that(vectors_disjoint(v1, v3))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v1 and v3 must be disjoint vectors but the following elements ",
      "are present in both: foo\n"
      ))

  err <- tryCatch({
    assertthat::assert_that(vectors_disjoint(v4, v5))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v4 and v5 must be disjoint vectors but the following elements ",
      "are present in both: 5, 7\n"
      ))

  df <- data.frame()
  err <- tryCatch({
    assertthat::assert_that(vectors_disjoint(v1, df))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    "Error: df must be a vector. Got: data.frame\n"
  )

})
test_that("is_integer_value", {
  expect_true(is_integer_value(1))
  expect_false(is_integer_value(1.01))
  expect_false(is_integer_value("1"))
  expect_false(is_integer_value(c(3, 4)))
  expect_true(is_integer_value(1, min = 1))
  expect_true(is_integer_value(1, max = 1))
  expect_true(is_integer_value(2, min = 1, max = 3))
  expect_false(is_integer_value(5, min = 1, max = 3))
  expect_false(is_integer_value(0, min = 1, max = 3))
  expect_true(is_integer_value(NULL, allow_null = TRUE))

  foo <- 2
  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, min = 3))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be an integer value in the range [3, inf). Got: 2\n")

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, max = 1))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be an integer value in the range (-inf, 1]. Got: 2\n")

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(foo, min = 3, max = 5))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be an integer value in the range [3, 5]. Got: 2\n")

  err <- tryCatch({
      assertthat::assert_that(is_integer_value(data.frame(), min = 3, max = 5))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: data.frame() must be an integer value in the range [3, 5].",
      " Got: structure(list(), .Names = character(0), row.names = ",
      "integer(0), class = \"data.frame\")\n"))

})

test_that("is_positive_real_value", {
  expect_true(is_positive_real_value(1))
  expect_true(is_positive_real_value(1.01))
  expect_false(is_positive_real_value("1"))
  expect_false(is_positive_real_value(c(1.2, 1.3)))

  foo <- -1.5
  err <- tryCatch({
      assertthat::assert_that(is_positive_real_value(foo))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a positive real value. Got: -1.5\n")

})

test_that("is_logical_value", {
  expect_true(is_logical_value(FALSE))
  expect_false(is_logical_value(c(FALSE, FALSE)))
  expect_false(is_logical_value(""))
  foo <- -1.5
  err <- tryCatch({
      assertthat::assert_that(is_logical_value(foo))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: foo must be a single logical value. Got: -1.5\n")
})

test_that("is_data_frame", {
  df <- data.frame(
      foo = c(1, 2, 3),
      bar = c(3, 4, 5)
  )
  empty <- data.frame()
  expect_true(is_data_frame(df))
  expect_true(is_data_frame(df, allow_na = TRUE))
  expect_true(is_data_frame(NA, allow_na = TRUE))
  expect_false(is_data_frame(NA, allow_na = FALSE))
  expect_true(is_data_frame(df, required_colnames = c("foo")))
  expect_false(is_data_frame(df, required_colnames = c("baz")))
  expect_false(is_data_frame(df, exact_colnames = c("foo", "bar", "baz")))
  expect_false(is_data_frame(df, exact_colnames = c("foo")))
  expect_true(is_data_frame(df, exact_colnames = c("foo", "bar")))

  err <- tryCatch({
      assertthat::assert_that(is_data_frame(df, required_colnames = c("baz")))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0("Error: df must be a data frame with required colnames 'baz'. ",
          "Got: structure(list(foo = c(1, 2, 3), bar = c(3, 4, 5)), ",
           "class = \"data.frame\", row.names = c(NA, -3L))\n")
    )

  err <- tryCatch({
      assertthat::assert_that(is_data_frame(df,
        exact_colnames = c("foo", "bar", "baz")))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0("Error: df must be a data frame with exact colnames ",
      "'foo', 'bar', 'baz'. ",
      "Got: structure(list(foo = c(1, 2, 3), bar = c(3, 4, 5)), ",
      "class = \"data.frame\", row.names = c(NA, -3L))\n")
    )

  err <- tryCatch({
      assertthat::assert_that(is_data_frame(df,
        exact_colnames = c("foo", "bar", "baz"),
        exact_rownames = c("one", "two")))
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0("Error: df must be a data frame with exact rownames ",
      "'one', 'two' and with exact colnames 'foo', 'bar', 'baz'. ",
      "Got: structure(list(foo = c(1, 2, 3), bar = c(3, 4, 5)), ",
      "class = \"data.frame\", row.names = c(NA, -3L))\n")
    )
})

test_that("is_r6_class", {
  cls <- R6::R6Class("mycls", list())
  expect_true(is_r6_class(cls, "mycls"))
  expect_false(is_r6_class(cls, "whatever"))
  expect_false(is_r6_class(NULL, "mycls"))
  expect_false(is_r6_class(instance, NULL))
})

test_that("is_r6_instance", {
  cls <- R6::R6Class("mycls", list())
  instance <- cls$new()
  expect_true(is_r6_instance(instance, "mycls"))
  expect_false(is_r6_instance(instance, "whatever"))
  expect_false(is_r6_instance(NULL, "mycls"))
  expect_false(is_r6_instance(instance, NULL))
  expect_true(is_r6_instance(NULL, "mycls", allow_null = TRUE))

  err <- tryCatch({
    assertthat::assert_that(is_r6_instance(NULL, "mycls"))
    NULL
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    "Error: NULL must be an instance of R6 class mycls. Got: NULL\n"
    )

  err <- tryCatch({
    assertthat::assert_that(is_r6_instance(list(), "mycls", allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    })

  expect_equal(
    as.character(err),
    paste0(
      "Error: list() must be an instance of R6 class mycls or NULL. ",
      "Got: list()\n")
    )

})

test_that("any_satisfied", {
  expect_true(any_satisfied(is_string_value(""), is_logical_value("")))
  expect_true(any_satisfied(is_string_value(FALSE), is_logical_value(FALSE)))
  expect_false(
    any_satisfied(
      is_string_value(list()),
      is_logical_value(list())
    )
  )
  err <- tryCatch({
    assertthat::assert_that(
      any_satisfied(
        is_string_value(list()),
        is_logical_value(list())
      )
    )
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste(
      "Error: any_satisfied(is_string_value(list()),",
      "is_logical_value(list())): none of the possible",
      "options could be satisfied.\n"
    )
  )

})

test_that("is_na_value", {
  expect_true(is_na_value(NA))
  expect_false(is_na_value(c(NA, NA)))
  expect_false(is_na_value("hello"))

  err <- tryCatch({
    foo <- 3
    assertthat::assert_that(is_na_value(foo))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    "Error: foo must be NA. Got: 3\n")
})

test_that("is_S3_instance", {
  s <- list(name = "John", age = 21)
  class(s) <- c("InternationalStudent", "Student")
  expect_true(is_s3_instance(s, "InternationalStudent"))
  expect_true(is_s3_instance(s, "Student"))
  expect_false(is_s3_instance(s, "list"))
  expect_false(is_s3_instance(NULL, "Student"))
  expect_true(is_s3_instance(NULL, "Student", allow_null = TRUE))

  err <- tryCatch({
      assertthat::assert_that(is_s3_instance(NULL, "Student"))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: NULL must be an instance of S3 class Student. Got: NULL\n")

  err <- tryCatch({
      assertthat::assert_that(is_s3_instance(
        list(), "Student", allow_null = TRUE))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )

  expect_equal(
    as.character(err),
    paste0(
      "Error: list() must be an instance of S3 class Student or NULL. ",
      "Got: list()\n"))
})

test_that("is_list", {
  # this is expected. data frame is not inheriting from list.
  expect_false(is_list(data.frame()))

  expect_false(is_list(NULL))
  expect_true(is_list(NULL, allow_null = TRUE))
  expect_true(is_list(list()))

  expect_false(is_list(list(foo = "bar"), required_names = c("foo", "yup")))
  expect_true(is_list(list(foo = "bar", yup = "xxx"), required_names = c("foo")))
  expect_true(is_list(list(foo = "bar", yup = "xxx"), exact_length = 2))
  expect_false(is_list(list(foo = "bar", yup = "xxx"), exact_length = 3))
  expect_false(is_list(list(foo = "bar", yup = "xxx"), exact_length = 1))

  err <- tryCatch({
    assertthat::assert_that(is_list(3, allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(as.character(err), "Error: 3 must be a list or NULL. Got: 3\n")
})

test_that("is_function", {
  foo <- function(x, y, z) {
  }

  expect_true(is_function(foo))
  expect_false(is_function(foo, args = c("x", "z", "y")))
  expect_false(is_function(foo, args = c("x", "y")))
  expect_true(is_function(foo, args = c("x", "y", "z")))
  expect_false(is_function(""))

  expect_false(is_function(tibble::tibble()))

  err <- tryCatch({
    assertthat::assert_that(is_function(foo, args = c("x", "z", "y")))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    "Error: foo must be a function with arguments x, z, y. Got: foo\n")
})

test_that("is_real_value", {
  expect_true(is_real_value(10.0))
  expect_true(is_real_value(-0.1))
  expect_true(is_real_value(0.0, min = 0.0))
  expect_false(is_real_value(0.0, min = 0.0, inclusive_min = FALSE))
  expect_true(is_real_value(0.0, max = 0.0))
  expect_false(is_real_value(0.0, max = 0.0, inclusive_max = FALSE))
  expect_false(is_real_value("hello"))
  expect_false(is_real_value(c(0.0, 0.1)))
  expect_false(is_real_value(NULL))
  expect_true(is_real_value(NULL, allow_null = TRUE))
  expect_false(is_real_value(NA_real_))
  expect_true(is_real_value(NA_real_, allow_na = TRUE))

  foo <- "hello"
  err <- tryCatch({
      assertthat::assert_that(is_real_value(foo))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: foo must be a single real value. Got: \"hello\"\n")
})

test_that("is_probability_value", {
  expect_true(is_probability_value(1.0))
  expect_true(is_probability_value(0.5))
  expect_true(is_probability_value(0.0))
  expect_false(is_probability_value(-0.1))
  expect_false(is_probability_value(1.1))
  expect_false(is_probability_value("hello"))
  expect_false(is_probability_value(c(0.0, 0.1)))

  foo <- -0.3
  err <- tryCatch({
      assertthat::assert_that(is_probability_value(foo))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: foo must be a single probability value in the interval [0.0, 1.0]. Got: -0.3\n")
})

test_that("mutually_exclusive", {
  expect_true(mutually_exclusive(NULL, NULL, NULL))
  expect_true(mutually_exclusive(1, NULL, NULL))
  expect_true(mutually_exclusive(TRUE, NULL, NULL))
  expect_true(mutually_exclusive(NULL, TRUE, NULL))
  expect_true(mutually_exclusive(NULL, NULL, 3))
  expect_false(mutually_exclusive(TRUE, NULL, 24))
  expect_false(mutually_exclusive(TRUE, "hello", 24))

  foo <- -0.3
  bar <- NULL
  baz <- 5
  err <- tryCatch({
      assertthat::assert_that(mutually_exclusive(foo, bar, baz))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: 'foo', 'baz' must be mutually exclusive. Got -0.3, 5\n")
})

test_that("data_frames_same_colnames", {
  df1 <- data.frame(foo=c(1,2,3), bar=c(1,2,3))
  df2 <- data.frame(foo=c(1,2,3))
  df3 <- data.frame(foo=c(1,2,3), bar=c(1,2,3), baz=c(1,2,3))

  expect_true(data_frames_same_colnames(df1, df1))
  expect_false(data_frames_same_colnames(df1, df2))
  expect_false(data_frames_same_colnames(df1, df3))

  err <- tryCatch({
      assertthat::assert_that(data_frames_same_colnames(df1, df2))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: data frames df1 and df2 must have the exact same column names.\n")

})

test_that("vector_allowed_values", {
  v1 <- c("foo", "bar", "bar", NA_character_)

  expect_true(vector_allowed_values(v1, c("foo", "bar", "baz", NA_character_)))
  expect_true(vector_allowed_values(v1, c("foo", "bar", NA_character_)))
  expect_false(vector_allowed_values(v1, c("foo")))

  err <- tryCatch({
      assertthat::assert_that(vector_allowed_values(
        v1,
        c("foo", "quux", NA_character_)))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: v1 must be a vector containing only elements from the ",
      "following list: foo, quux, NA\n"
     ))
})

test_that("is_one_of", {
  options <- c("foo", "bar", "baz")

  expect_true(is_one_of("foo", options))
  expect_false(is_one_of("fux", options))
  expect_false(is_one_of("1", c(1, 2, 3)))
  expect_false(is_one_of(c(1, 2), c(1, 2, 3)))

  val <- "x"
  err <- tryCatch({
      assertthat::assert_that(is_one_of(
        val,
        c("foo", "quux", NA_character_),
        allow_null = TRUE))
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: val must be one of the following: 'foo', 'quux', 'NA' or NULL. ",
      "Got: \"x\"\n"
     ))
})

test_that("is_binary_vector", {
  v1 <- c(0, 1, 1, 1, 0)
  v2 <- c(0, 1, 1, 2, 0)

  expect_true(is_binary_vector(v1))
  expect_false(is_binary_vector(v2))

  err <- tryCatch({
      assertthat::assert_that(is_binary_vector(v2))
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: v2 must be a vector of binary values (0 or 1)\n"
     ))
})

test_that("is_binary_vector with NA", {
  v1 <- c(0, 1, NA, 1, 0)

  expect_false(is_binary_vector(v1))
  expect_true(is_binary_vector(v1, allow_na_values = TRUE))

})

test_that("is_binary_vector allow_degenerate", {
  v1 <- c(1, 1, 1, 1, 1, NA)
  v2 <- c(0, 0, 0, 0)

  expect_true(is_binary_vector(v1, allow_na_values = TRUE))
  expect_false(is_binary_vector(
    v1, allow_na_values = TRUE, allow_degenerate = FALSE))

  expect_true(is_binary_vector(v2))
  expect_false(is_binary_vector(v2, allow_degenerate = FALSE))

  err <- tryCatch({
      assertthat::assert_that(is_binary_vector(v1, allow_degenerate = FALSE))
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: v1 must be a non-degenerate vector of binary values (0 or 1)\n"
     ))
})

test_that("is_real_vector", {
  v <- c(1.2, 1.3, 3.0)
  v2 <- c("hello", "hi")
  expect_true(is_real_vector(v))
  expect_false(is_real_vector(NULL))
  expect_false(is_real_vector(v2))
  expect_false(is_real_vector(v, exact_length = 4))
  expect_false(is_real_vector(v, min_length = 4))
  expect_true(is_real_vector(v, max_length = 4))
  expect_false(is_real_vector(v, max_length = 2))
  expect_true(is_real_vector(v, exact_length = 3, max_length = 2))

  v2 <- 1.0
  expect_true(is_real_vector(v2))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0("Error: v must be a vector of real numbers of exact length 4. ",
    "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of real numbers of length not less than 4. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of real numbers of length not ",
      "greater than 2. ",
      "Got: c(1.2, 1.3, 3)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_real_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of real numbers of length ",
      "between 4 and 8 inclusive. ",
      "Got: c(1.2, 1.3, 3)\n"))
})

test_that("is_real_vector with NA", {
  v <- c(1.2, NA, 3.0)
  expect_false(is_real_vector(v))
  expect_true(is_real_vector(v, allow_na_values = TRUE))
})

test_that("is_nonnegative_integer_vector", {
  v <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)

  expect_true(is_non_negative_integer_vector(v))
  expect_false(is_non_negative_integer_vector(NULL))
  expect_false(is_non_negative_integer_vector(v2))
  expect_false(is_non_negative_integer_vector(v3))

  expect_false(is_non_negative_integer_vector(v, exact_length = 4))
  expect_false(is_non_negative_integer_vector(v, min_length = 4))
  expect_true(is_non_negative_integer_vector(v, max_length = 4))
  expect_false(is_non_negative_integer_vector(v, max_length = 2))
  expect_true(is_non_negative_integer_vector(
    v, exact_length = 3, max_length = 2))

  err <- tryCatch({
    assertthat::assert_that(
      is_non_negative_integer_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer numbers of exact ",
      "length 4. Got: c(1, 13, 0)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_integer_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer numbers of length ",
      "not less than 4. Got: c(1, 13, 0)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_integer_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer numbers of ",
      "length not greater than 2. Got: c(1, 13, 0)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_non_negative_integer_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of non negative integer numbers of length ",
      "between 4 and 8 inclusive. ",
      "Got: c(1, 13, 0)\n"))
})

test_that("is_nonnegative_integer_vector with NA", {
  v <- c(1, NA, 0)

  expect_false(is_non_negative_integer_vector(v))
  expect_true(is_non_negative_integer_vector(v, allow_na_values=TRUE))

})

test_that("is_integer_vector", {
  v <- c(1, 13, 0)
  v2 <- c(1, 5.2, 3)
  v3 <- c(3, -4, 3)

  expect_true(is_integer_vector(v))
  expect_false(is_integer_vector(NULL))
  expect_false(is_integer_vector(v2))
  expect_true(is_integer_vector(v3))

  expect_false(is_integer_vector(v, exact_length = 4))
  expect_false(is_integer_vector(v, min_length = 4))
  expect_true(is_integer_vector(v, max_length = 4))
  expect_false(is_integer_vector(v, max_length = 2))
  expect_true(is_integer_vector(
    v, exact_length = 3, max_length = 2))

  err <- tryCatch({
    assertthat::assert_that(
      is_integer_vector(v, exact_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(as.character(err),
    paste0(
      "Error: v must be a vector of integer values of exact ",
      "length 4. Got: c(1, 13, 0)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_integer_vector(v, min_length = 4))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of integer values of length ",
      "not less than 4. Got: c(1, 13, 0)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_integer_vector(v, max_length = 2))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of integer values of ",
      "length not greater than 2. Got: c(1, 13, 0)\n"))

  err <- tryCatch({
    assertthat::assert_that(is_integer_vector(v,
      min_length = 4, max_length = 8))
    NULL
  },
  error = function(e) {
    return(e)
  })

  expect_equal(
    as.character(err),
    paste0(
      "Error: v must be a vector of integer values of length ",
      "between 4 and 8 inclusive. ",
      "Got: c(1, 13, 0)\n"))
})

test_that("is_integer_vector with NA", {
  v <- c(1, NA, 0)

  expect_false(is_integer_vector(v))
  expect_true(is_integer_vector(v, allow_na_values = TRUE))

})

test_that("is_vector_without_na", {
  expect_false(is_vector_without_na(c(1, NA, 2)))
  expect_true(is_vector_without_na(c(1, 2)))

})

test_that("is_vector_all_na", {
  expect_false(is_vector_all_na(c(1, NA, 2)))
  expect_true(is_vector_all_na(c(NA, NA)))

})

test_that("vector_value_occurrences", {
  vec <- c(1, 2, 3)
  val <- 3
  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(vec, val, exact_occurrences = 2)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' exactly 2 times. ",
      "Found it 1 times.\n"
    )
  )

  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(vec, val, min_occurrences = 2)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' no less than ",
      "2 times inclusive. Found it 1 times.\n"
    )
  )

  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(
          vec, val, min_occurrences = 2, max_occurrences = 4)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' between 2 and 4 ",
      "times inclusive. Found it 1 times.\n"
    )
  )

  vec <- c(1, 2, 3, 3, 3, 3)
  val <- 3

  err <- tryCatch({
      assertthat::assert_that(
        vector_value_occurrences(
          vec, val, max_occurrences = 3)
      )
      NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    paste0(
      "Error: vec must be a vector containing value '3' no more than ",
      "3 times inclusive. Found it 4 times.\n"
    )
  )

})

test_that("is_factor", {
  expect_false(is_factor(data.frame()))
  expect_false(is_factor(NULL))
  expect_true(is_factor(NULL, allow_null = TRUE))
  expect_true(is_factor(factor(c("foo", "bar"))))

  expect_false(is_factor(factor(c("foo", "bar")), exact_levels = c("foo", "yup")))
  expect_true(is_factor(factor(c("foo", "bar", "bar")), exact_levels = c("bar", "foo")))
  expect_true(is_factor(factor(c("foo", "bar", "bar")), exact_length = 3))
  expect_false(is_factor(factor(c("foo", "bar")), exact_length = 3))
  expect_false(is_factor(factor(c("foo", "bar", NA_character_)), exact_length = 3))

  err <- tryCatch({
    assertthat::assert_that(is_factor(3, exact_levels = c("bar", "foo"), allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: 3 must be a factor with exact levels 'bar', 'foo' or NULL. Got: 3\n"
  )
})
