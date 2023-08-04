# Adds quotes '' but only if the passed value is a character.
# Otherwise, returns the value as is.
quote_if_char <- function(value) {
  if (is(value, "character")) {
    return(paste0("'", value, "'"))
  }
  return(value)
}

# The most naive pluralization function
pluralize_if <- function(word, num) {
  if (abs(num) == 1) {
    return(word)
  }
  if (word == "is") {
    return("are")
  }
  return(paste0(word, "s"))
}

# Flattens a vector and converts each element into a character for
# representation. Returns a collapsed string (with optional start and end
# parentheses). If the vector is a character vector, the quote_if_char
# option specifies if the char elements must be quoted in single quotes or not.
flatten_vector <- function(vector, quote_if_char = TRUE, parenthesis = FALSE) {
  v <- lapply(vector, function(x) {
    if (is.character(x) && is.na(x)) {
        return(NA_character_)
    } else if (is.numeric(x)) {
      return(as.character(x))
    } else {
      quotes_str <- ifelse(quote_if_char, "'", "")
      return(paste0(quotes_str, x, quotes_str))
    }
  })
  msg <- paste0(
    ifelse(parenthesis, "(", ""),
    paste0(v, collapse = ", "),
    ifelse(parenthesis, ")", "")
  )

  return(msg)
}
