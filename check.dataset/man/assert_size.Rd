% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_table_consistency_checks.R
\name{assert_size}
\alias{assert_size}
\title{Assert table size}
\usage{
assert_size(df, direction, size)
}
\arguments{
\item{df}{a data.frame (local or remote)}

\item{direction}{mathematical operator for comparison.}

\item{size}{numeric value to compare against size.}
}
\value{
Errors if assert fails. Returns number of rows invisibly if passed.
}
\description{
Asserts number of rows in table is relative to size.
}
\examples{
data(iris)
assert_size(iris, ">=", 100)
# no error - iris has 150 rows

}
