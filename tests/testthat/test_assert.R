test_that("assert", {
  err <- tryCatch({
    assert(is_factor(3, exact_levels = c("bar", "foo"), allow_null = TRUE))
    NULL
    },
    error = function(e) {
      return(e)
    }
  )
  expect_equal(
    as.character(err),
    "Error: 3 must be a factor with exact levels ('bar', 'foo') with no NAs; or NULL. Got: 3\n"
  )
})