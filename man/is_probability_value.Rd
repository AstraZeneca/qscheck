% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/real.R
\name{is_probability_value}
\alias{is_probability_value}
\title{Check if the passed entity is a single probability value, that is, a
floating point value in the interval [0.0, 1.0].}
\usage{
is_probability_value(value)
}
\arguments{
\item{value}{the value to check}
}
\description{
Check if the passed entity is a single probability value, that is, a
floating point value in the interval [0.0, 1.0].
}
\examples{
\dontrun{
# For assertion
assertthat::assert_that(qscheck::is_probability_value(value))
# For check
if (qscheck::is_probability_value(value)) {}
}

}
\concept{real}
