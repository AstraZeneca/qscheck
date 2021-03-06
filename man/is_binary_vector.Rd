% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{is_binary_vector}
\alias{is_binary_vector}
\title{Checks if a vector contains only binary values (0 or 1)}
\usage{
is_binary_vector(v, allow_na_values = FALSE, allow_degenerate = TRUE)
}
\arguments{
\item{v}{the vector}

\item{allow_na_values}{if the vector can contain NA values. Default FALSE}

\item{allow_degenerate}{if TRUE, the vector can contain only one value class
(e.g. all the non-NA values are 0, and there's not a single 1, or
vice-versa). If FALSE, such vectors will be rejected.}
}
\description{
Checks if a vector contains only binary values (0 or 1)
}
\examples{
\dontrun{
# For assertion.
# Will fail if v contains any value that is not either 0 or 1
assertthat::assert_that(qscheck::is_binary_vector(v))
# For check
if (qscheck::is_binary_vector(v)) {}
}

}
\concept{vector}
