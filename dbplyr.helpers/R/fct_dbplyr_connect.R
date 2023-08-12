################################################################################
# Description: dbplyr database connection functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## Print connection guidance ---------------------------------------------- ----
#' Print user guidance on database connections
#' 
#' Connecting to a database can be a frustrating task the first time around.
#' This is often a bespoke task that depends on the exact configuration of the
#' database.
#' 
#' This function returns example code that we found useful when connecting to,
#' and disconnecting from, a MS SQL Server. It is intended to serve as a guide
#' for arranging this in your own work.
#' 
#' @return NULL
#' 
#' @export
#' 
display_connection_guidance = function(){
  
  msg = paste(
    "## connect ------------------------------------",
    "",
    "# connection string",
    "con_str = \"DRIVER=ODBC Driver 17 for SQL Server; Trusted_Connection=Yes; DATABASE={database}; SERVER={server},{port};\"",
    "# create connection",
    "db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = con_str)",
    "",
    "## analysis -----------------------------------",
    "",
    "remote_tbl = create_access_point(db_conn, tbl_name = \"my_table\")",
    "",
    "",
    "",
    "## disconnect ---------------------------------",
    "",
    "DBI::dbDisconnect(db_conn)",
    "",
    sep = "\n"
  )
  
  cat(msg)
  return(invisible(NULL))
}

## Access remote SQL table ------------------------------------------------ ----
#' Access remote table
#' 
#' Maps an SQL table as a remote data frame in R, allowing R to run queries on
#' the the server.
#' 
#' Use this in place of loading a table into R. It will be treated as an R data
#' frame but the data will remain on the server (instead of in R memory).
#' 
#' The same `db_connection` must be used for each remote table. Otherwise it is
#' not possible to join tables together. Tables from different databases on the
#' same SQL Server can be accessed by the same connection.
#' 
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param tbl_name the name of the Table or View in SQL. Warns is not delimited
#' in square brackets.
#' 
#' If your table does not exist you will receive an error.
#' If you do not have permission to access all the table columns you will
#' receive an error. In this case, the recommended solution is to create a View
#' that only selects the columns you have permissions for.
#' 
#' @return a remote data frame
#' 
#' @export
create_access_point = function(db_connection, db = "[]", schema = "[]", tbl_name) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  stopifnot(table_or_view_exists_in_db(db_connection, db, schema, tbl_name))
  
  if(any(grepl("SQLite", class(db_connection)))) {
    tbl_name = remove_delimiters(tbl_name, "[]")
  }
  
  tryCatch(
    {
      # access table
      if (nchar(schema) > 2 | nchar(db) > 2) {
        table_access = dplyr::tbl(db_connection, from = dbplyr::in_schema(db_schema(db, schema), tbl_name))
      } else {
        table_access = dplyr::tbl(db_connection, from = tbl_name)
      }
    },
    error = function(cond) {
      message("Error accessing existing SQL table, you may lack permissions")
      message("Original error message:")
      message(cond)
      return(NA)
    }
  )
  return(table_access)
}
