% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_table_consistency_checks.R
\name{assert_num_distinct}
\alias{assert_num_distinct}
\title{Assert number of distinct values}
\usage{
assert_num_distinct(df, col_name, direction, size)
}
\arguments{
\item{df}{a data.frame (local or remote).}

\item{col_name}{the name(s) of columns to count distinct values/combinations
of. Errors if any \code{col_name} is not a column name of \code{df}.}

\item{direction}{mathematical operator for comparison.}

\item{size}{numeric value to compare against size.}
}
\value{
Errors if assert fails. Else, number of distinct values invisibly
}
\description{
Assert number of unique values in col_name (or a combination of col_names)
of df is in direction relative to size
}
\examples{
data(iris)
assert_num_distinct(iris, "Species", "!=", 1)

}
