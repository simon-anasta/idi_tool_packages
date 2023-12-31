% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_dbplyr_write.R
\name{append_database_table}
\alias{append_database_table}
\title{Append rows to an existing table}
\usage{
append_database_table(
  table_to_append,
  db_connection,
  db = "[]",
  schema = "[]",
  tbl_name,
  list_of_columns,
  query_path = NA
)
}
\arguments{
\item{table_to_append}{an existing remote table that contains the rows to
append.}

\item{db_connection}{the open connection to close.
Often created using \code{create_database_connection}.}

\item{db}{the name of the database. Can be different from the database used
when creating \code{db_connection} but must be on the same server. Warns if not
delimited in square brackets.}

\item{schema}{the name of the schema containing the Table or View. Warns if
not delimited in square brackets.}

\item{tbl_name}{the name of the Table in SQL. Warns is not delimited
in square brackets.}

\item{list_of_columns}{a list of columns found in both tables. Determine
which columns from \code{table_to_append} are appended.}

\item{query_path}{If provided will attempt to save a copy of the SQL code
sent to/executed on the database to the provided folder. Save occurs before
execution, hence useful for debugging.}
}
\value{
the result from executing the command using \code{DBI::dbExecute}
(seldom used).

A common error occurs when character strings in the appended table exceed the
length of the existing VARCHAR(n) limit in the receiving table. I.e. avoid
appending a 5 character string like 'ABCDE' into a VARCHAR(3) column.
}
\description{
Given a table from a database connection, append it to an existing table
using the INSERT INTO ... (COLUMN NAMES) SELECT ... FROM ... pattern.
}
\details{
But source tables must exist and be database tables. If the receiving table
does not already exist, then you can create it with \code{create_table}. If
wanting to append a local R data frame, then you can first copy it to the
database using \code{copy_r_to_sql}.
}
