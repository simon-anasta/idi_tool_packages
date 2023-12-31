% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_overview_dataset.R
\name{filter_to_limited_number_of_rows}
\alias{filter_to_limited_number_of_rows}
\title{Filter to a limited number of rows}
\usage{
filter_to_limited_number_of_rows(df, row_limit = 10000, id_column = NA)
}
\arguments{
\item{df}{a data.frame (local or remote)}

\item{row_limit}{maximum number of rows in the returned table}

\item{id_column}{the column of a numeric id to be used for filtering
(optional - see logic for behavior). This column is assumed to be numeric
and integer.}
}
\value{
a data.frame (in local R memory) containing at most \code{row_limit} rows.
}
\description{
Given a data.frame, filter it to the specified maximum number of rows.
Designed to work with both local and remote data.frames.
}
\details{
Expected use case is sampling data for exploration or to minimize run time
of calculations during development.

Logic:
\itemize{
\item If no id_column is specified, then return the first rows
\item If an id_column is specified, then select a random sample that will
return approximately the limit number of rows. This is done via modulus
of the ID number.
}

Assumes that id_column is independent of data. filter is likely to produce
non-representative results if id_column is not random with respect to data.
}
