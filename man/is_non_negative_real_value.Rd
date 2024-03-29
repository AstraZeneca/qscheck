% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/real_value.R
\name{is_non_negative_real_value}
\alias{is_non_negative_real_value}
\title{Check if the passed entity is a single floating point non-negative value.}
\usage{
is_non_negative_real_value(value, allow_na = FALSE, allow_null = FALSE)
}
\arguments{
\item{value}{the value to check}

\item{allow_na}{if true, accept a value that is NA.}

\item{allow_null}{if true, accept a value that is NULL.}
}
\description{
Check if the passed entity is a single floating point non-negative value.
}
\examples{
\dontrun{
# For assertion
assertthat::assert_that(qscheck::is_non_negative_real_value(value))
# For check
if (qscheck::is_non_negative_real_value(value)) {}
}

}
\concept{real}
