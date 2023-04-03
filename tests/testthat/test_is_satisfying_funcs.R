test_that("satisfyingFuncsOnePasses", {
  expect_true(
    is_satisfying_funcs(
      3,
      function(x) {
        x < 4
      }
    )
  )

  expect_true(
    is_satisfying_funcs(
      3,
      function(x) {
        if (x < 4) {
          return(success())
        } else {
          return(failure())
        }
      }
    )
  )

  assert(
    is_satisfying_funcs(
      3,
      function(x) {
        if (x < 4) {
          return(success())
        } else {
          return(failure())
        }
      }
    )
  )

  assert(
    is_satisfying_funcs(
      3,
      function(x) {
        x < 4
      }
    )
  )
})


test_that("satisfyingFuncsMultiPasses", {
  expect_true(
    is_satisfying_funcs(
      3,
      list(
        function(x) {
          x < 4
        },
        function(x) {
          x > 2
        }
      )
    )
  )

  expect_true(
    is_satisfying_funcs(
      3,
      list(
        function(x) {
          if (x < 4) {
            return(success())
          } else {
            return(failure())
          }
        },
        function(x) {
          x > 2
        }
      )
    )
  )

  expect_true(
    is_satisfying_funcs(3, list(
        less_than_five = function(x) {
          x < 5
        },
        more_than_two = function(x) {
          x > 2
        })
    )
  )

  assert(
    is_satisfying_funcs(
      3,
      list(
        function(x) {
          x < 4
        },
        function(x) {
          x > 2
        }
      )
    )
  )


})

test_that("satisfyingFuncsOneFails", {
  expect_error(
    assert(is_satisfying_funcs(3, function(x) {
      if (x > 5) {
        success()
      } else {
        failure("Value must be greater than five")
      }
    })),
    paste0(
      "Argument '3' must satisfy all conditions in the check\\. ",
      "Check function number 1 \\(tag: 1\\) failed: Value must be ",
      "greater than five\\."
    )
  )

  expect_error(
    assert(is_satisfying_funcs(3, function(x) {
      x > 5
    })),
    paste0(
      "Argument '3' must satisfy all conditions in the check\\. ",
      "Check function number 1 \\(tag: 1\\) failed\\."
    )
  )
})

test_that("satisfyingFuncsMultiFails", {
  expect_error(
    assert(
      is_satisfying_funcs(8, list(
        less_than_five = function(x) {
          x < 5
        },
        more_than_two = function(x) {
          x > 2
        })
      )
    ),
    paste0(
      "Argument '8' must satisfy all conditions in the check\\. ",
      "Check function number 1 \\(tag: less_than_five\\) failed\\."
    )
  )

  expect_error(
    assert(
      is_satisfying_funcs(8, list(
        less_than_five = function(x) {
          if (x < 5) {
            return(success())
          } else {
            return(failure("Number must be less than five"))
          }
        },
        more_than_two = function(x) {
          x > 2
        })
      )
    ),
    paste0(
      "Argument '8' must satisfy all conditions in the check\\. ",
      "Check function number 1 \\(tag: less_than_five\\) failed: ",
      "Number must be less than five\\."
    )
  )

  res <- inspect_is_satisfying_funcs(8, list(
        less_than_five = function(x) {
          if (x < 5) {
            return(success())
          } else {
            return(failure("Number must be less than five"))
          }
        },
        more_than_two = function(x) {
          x > 2
        })
      )

  expect_false(res$valid)
  expect_false(res$subreport$valid)
  expect_equal(res$subreport$reason, "Number must be less than five")
  expect_equal(res$tag, "less_than_five")

  expect_error(
    assert(
      is_satisfying_funcs(1, list(
        less_than_five = function(x) {
          if (x < 5) {
            return(success())
          } else {
            return(failure("Number must be less than five"))
          }
        },
        more_than_two = function(x) {
          x > 2
        })
      )
    ),
    paste0(
      "Argument '1' must satisfy all conditions in the check\\. ",
      "Check function number 2 \\(tag: more_than_two\\) failed\\."
    )
  )
})
