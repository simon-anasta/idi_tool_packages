################################################################################
# Description: dbplyr database connection functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Create connection string ----------------------------------------------- ----
#'
#' ODBC connections require a connection string to define the connection
#' between the database and the access point. This string has a default
#' format that this function satisfies.
#'
#' This function does not need to be called directly to use the utility
#' functions. It is called by create_database_connection.
#'

#' Title
#'
#' @param server 
#' @param database 
#' @param port 
#'
#' @return the connection string to initiate a new database connection.
#' 
#' @export
set_connection_string = function(server, database, port = NA) {
  connstr = "DRIVER=ODBC Driver 17 for SQL Server; "
  connstr = paste0(connstr, "Trusted_Connection=Yes; ")
  connstr = paste0(connstr, "DATABASE=", database, "; ")
  connstr = paste0(connstr, "SERVER=", server)
  if (!is.na(port)) {
    connstr = paste0(connstr, ", ", port)
  }
  return(connstr)
}

## Create database connection point --------------------------------------- ----
#'
#' Any arguments passed to the function need to be named.
#' Default values are set at the top of this script.
#'
#' A single connection can access multiple associated databases.
#' We recommend using a single connection to access all tables
#' because attempts to join tables from different connections
#' (even if they are in the same database) perform poorly if
#' at all.
#'

#' Title
#'
#' @param ... 
#' @param server 
#' @param database 
#' @param port 
#'
#' @return the database connection for use in all other functions
#' 
#' @export
create_database_connection = function(..., server, database, port) {
  # checks
  if(length(list(...)) != 0){
    stop("all database connection arguments must be named")
  }
  stopifnot(is.character(server))
  stopifnot(is.character(database))
  stopifnot(is.na(port) | is.character(port) | is.numeric(port))
  
  server = remove_delimiters(server, "[]")
  database = remove_delimiters(database, "[]")
  
  # connect
  connection_string = set_connection_string(server, database, port)
  db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
}

## Close an open database connection -------------------------------------- ----
#' Close an open database connection
#' 
#' Good practice is to close open connections once they are finished with.
#' Large numbers of open connections can degrade performance.
#' 
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' 
#' @export
close_database_connection = function(db_connection) {
  DBI::dbDisconnect(db_connection)
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
create_access_point = function(db_connection, db = "", schema = "", tbl_name) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  stopifnot(table_or_view_exists_in_db(db_connection, db, schema, tbl_name))
  
  tryCatch(
    {
      # access table
      if (nchar(schema) > 0 | nchar(db) > 0) {
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
