test_that("", {
  expect_error(
    assert(is_satisfying(3, function(x) {
      x > 5
    })),
    ""
  )

  expect_error(
    assert(is_satisfying(3, list(
        foobar = function(x) {
          x > 5
        })
    )),
    ""
  )

  expect_true(
    is_satisfying(3, list(
        foobar = function(x) {
          x < 5
        })
    )
  )

})
