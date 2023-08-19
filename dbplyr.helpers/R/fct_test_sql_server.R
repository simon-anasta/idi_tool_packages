################################################################################
# Description: Run SQL Server tests
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Run SQL Server tests --------------------------------------------------- ----
#' Run SQL Server-specific tests
#' 
#' The original version of this package was developed using SQL Server within
#' the data lab environment. As we do not have an SQL Server available for
#' this redevelopment we have used SQLite for our testing.
#' 
#' This function provides a way to run many of the original tests within an
#' SQL Server environment.
#' 
#' @param connection_string The connection string to connect to the database.
#' See `display_connection_guidance` for guidance on its creation. It is
#' recommended that the connection be to the database in `view_db`.
#' @param table_db The name of the database to test writing to.
#' @param view_db The name of the database to test creating views in.
#' @param our_schema The name of the schema in both databases for testing.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#'
#' @export
#' 
test_with_sql_server = function(
    connection_string,
    table_db = "[IDI_Sandpit]",
    view_db = "[IDI_UserCode]",
    our_schema,
    query_path = NA
){
  # validate inputs
  stopifnot(is.character(connection_string))
  warn_if_missing_delimiters(table_db, our_schema, "[]")
  warn_if_missing_delimiters(view_db, "[]", "[]")
  
  # path
  path = system.file("extdata", "SQL_server_tests", package = "dbplyr.helpers")
  
  # test
  testthat::test_dir(path)
}