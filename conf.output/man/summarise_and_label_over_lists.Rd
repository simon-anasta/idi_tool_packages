% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_summarise.R
\name{summarise_and_label_over_lists}
\alias{summarise_and_label_over_lists}
\title{Summarise results for all combinations in list}
\usage{
summarise_and_label_over_lists(
  df,
  group_by_list,
  summarise_list,
  make_distinct,
  make_count,
  make_sum,
  clean = "none",
  remove.na.from.groups = TRUE,
  query_path = NA
)
}
\arguments{
\item{df}{a data.frame to summarise. Can be in-memory or remote accessed
with dbplyr.}

\item{group_by_list}{a list where each entry is an array of column names to
group the table by. Each item in \code{group_by_list} will become an input to
\code{group_by_cols} in \code{summarise_and_label}.}

\item{summarise_list}{a list where each entry is the name of a column to
summarise. Each item in \code{summarise_list} will become an input to
\code{summarise_col} in \code{summarise_and_label}.}

\item{make_distinct}{T/F, whether the output should include a count of
distinct values from \code{summarise_col}.}

\item{make_count}{T/F, whether the output should include a count of values
from \code{summarise_col}.}

\item{make_sum}{T/F, whether the output should include the sum of values
from \code{summarise_col}.}

\item{clean}{hat cleaning, if any, to apply to the summarised column. Must
be one of \verb{\{"none", "na.as.zero", "zero.as.na"\}}. Defaults to \code{"none"}. See
details for explanation.}

\item{remove.na.from.groups}{T/F, whether missing values in the data should
be excluded when grouping for summarisation. For example, if grouping by age
should the output include a row for the records with no age.
Defaults to TRUE.}

\item{query_path}{If provided and data source is SQL will attempt to save a
copy of the SQL code sent to/executed on the database to the provided folder.
Save occurs before execution, hence useful for debugging.}
}
\value{
a local data.frame in long-thin format containing the all the
summarised results appended together in long-thin format.
}
\description{
Produces a summary of every variable given in \code{summarise_list} for every
group given in \code{group_by_list}. Does so in a way that is robust to whether
df is a local or a remote table. Returns a local table in long-thin format.
}
\details{
Output produced is consistent with running multiple \code{dplyr::group_by} and
\code{dplyr::summarise} commands. But produces many summaries with a single
command and appends their output.
}
