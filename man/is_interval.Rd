% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval.R
\name{is_interval}
\alias{is_interval}
\title{Check if the two values constitute an interval (low, high)}
\usage{
is_interval(low, high, allow_degenerate = TRUE)
}
\arguments{
\item{low}{the lowest end of the interval}

\item{high}{the highest end of the interval}

\item{allow_degenerate}{If TRUE, the case low == high is allowed.
Defaults to TRUE}
}
\description{
This routine returns False if high < low.
}
\examples{
\dontrun{
# For assertion
assertthat::assert_that(qscheck::is_interval(low, high))
# For check
if (qscheck::is_interval(low, high)) {}
}

}
\concept{real}
