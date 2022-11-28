snippet_null <- function(allow_null) {
  allow_null_msg <- ""
  if (allow_null) {
    allow_null_msg <- " or NULL"
  }
  return(allow_null_msg)
}

snippet_na <- function(allow_na) {
  allow_na_msg <- ""
  if (allow_na) {
    allow_na_msg <- " or NA"
  }
  return(allow_na_msg)

}
