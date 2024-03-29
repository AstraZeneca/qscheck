% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sets.R
\name{is_one_of}
\alias{is_one_of}
\title{Checks if the passed entity is one of the ones in the options vector.}
\usage{
is_one_of(value, options, allow_null = FALSE)
}
\arguments{
\item{value}{the value to check}

\item{options}{a vector of options.
Value must be one of these for the check not to fail}

\item{allow_null}{if TRUE, allow the value NULL. If FALSE (default) do not
allow NULL.}
}
\description{
Checks if the passed entity is one of the ones in the options vector.
}
\examples{
\dontrun{
# For assertion
assertthat::assert_that(qscheck::is_one_of(value, c("foo", "bar")))
# For check
if (qscheck::is_one_of(value, c("foo", "bar"))) {}
}

}
\concept{set}
