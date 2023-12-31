% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_table_consistency_checks.R
\name{check_size_missing}
\alias{check_size_missing}
\title{Check number of missing values}
\usage{
check_size_missing(df, col_name, direction, size)
}
\arguments{
\item{df}{a data.frame (local or remote).}

\item{col_name}{the name(s) of columns to count distinct values/combinations
of. Errors if any \code{col_name} is not a column name of \code{df}.}

\item{direction}{mathematical operator for comparison.}

\item{size}{numeric value to compare against size.}
}
\value{
T/F whether number of missing values in col_name (or cumulative
across col_names) of \code{df} is \code{direction} relative to \code{size}.
}
\description{
Check number of missing values
}
\examples{
data(iris)
check_size_missing(iris, c("Petal.Length", "Petal.Width"), "==", 0)

}
