% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{vector_value_occurrences}
\alias{vector_value_occurrences}
\title{Checks if a given value occurs a given number of times in a vector}
\usage{
vector_value_occurrences(
  vec,
  value,
  exact_occurrences = NULL,
  min_occurrences = NULL,
  max_occurrences = NULL
)
}
\arguments{
\item{vec}{The vector to check}

\item{value}{The value to check}

\item{exact_occurrences}{the exact number of times the value must be
present. Takes precedence over min/max_occurrences.}

\item{min_occurrences}{the minimum number of times (inclusive) the
value must be present.}

\item{max_occurrences}{the maximum number of times (inclusive) the
value must be present.}
}
\description{
Checks if a given value occurs a given number of times in a vector
}
\examples{
\dontrun{
# For assertion
assertthat::assert_that(
  qscheck::vector_value_occurrences(
    vec, value, min_occurrences = 2, max_occurrences = 3
  )
)
# For check
if (qscheck::vector_value_occurrences(
    vec, value, min_occurrences = 2, max_occurrences = 3
   )) {
}
}

}
\concept{vector}
