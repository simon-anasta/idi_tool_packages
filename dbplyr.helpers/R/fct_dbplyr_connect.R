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

## Inform user with time stamped measure ---------------------------------- ----


## Create connection string -----------------------------------------------------------------------
#'
#' ODBC connections require a connection string to define the connection
#' between the database and the access point. This string has a default
#' format that this function satisfies.
#'
#' This function does not need to be called directly to use the utility
#' functions. It is called by create_database_connection.
#'
set_connection_string <- function(server, database, port = NA) {
  connstr <- "DRIVER=ODBC Driver 17 for SQL Server; "
  connstr <- paste0(connstr, "Trusted_Connection=Yes; ")
  connstr <- paste0(connstr, "DATABASE=", database, "; ")
  connstr <- paste0(connstr, "SERVER=", server)
  if (!is.na(port)) {
    connstr <- paste0(connstr, ", ", port)
  }
  return(connstr)
}

## Create database connection point ---------------------------------------------------------------
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
create_database_connection <- function(..., server = NA, database = NA, port = NA) {
  # checks
  assert("odbc" %in% installed.packages(), "odbc package must be installed to connect to database")
  assert(length(list(...)) == 0, "all database connection arguments must be named")
  
  # default values
  if (is.na(server)) {
    server <- DEFAULT_SERVER
  }
  if (is.na(database)) {
    database <- DEFAULT_DATABASE
  }
  if (is.na(port)) {
    port <- DEFAULT_PORT
  }
  
  server <- remove_delimiters(server, "[]")
  database <- remove_delimiters(database, "[]")
  
  # connect
  connection_string <- set_connection_string(server, database, port)
  db_connection <- DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
}

## Close an open database connection --------------------------------------------------------------
#'
#' Good practice is to close open connections once they are finished with.
#' Large numbers of open connections can degrade performance.
#'
close_database_connection <- function(db_connection) {
  DBI::dbDisconnect(db_connection)
}

## Map SQL table for access in R ------------------------------------------------------------------
#' Creates access point for R to run queries on SQL server.
#'
#' Use this in place of loading a table into R. It will be treated as an R dataframe
#' but the data will remain on the SQL server (instead of in R memory).
#'
#' The same db_connection must be used foreach table access point. Otherwise
#' you will not be able to join tables together. Tables from different databases
#' within the same server can be accessed bythe same connection.
#'
#' If table does not exist you will receive an error.
#' If you do not have permission to access to all the table columns you will receive an error.
#' Recommended solution: Create a View that only selects columns you have permission for.
#'
create_access_point <- function(db_connection, db, schema, tbl_name) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  
  tryCatch(
    {
      # access table
      if (nchar(schema) > 0 | nchar(db) > 0) {
        table_access <- dplyr::tbl(db_connection, from = dbplyr::in_schema(db_schema(db, schema), tbl_name))
      } else {
        table_access <- dplyr::tbl(db_connection, from = tbl_name)
      }
    },
    error = function(cond) {
      message(
        "Error accessing SQL table, you might lack permission for all columns of this table.\n",
        "Recommended solution: Create a View of the columns you have permission for and access this."
      )
      message("Here is the original error message:")
      message(cond)
      return(NA)
    }
  )
  return(table_access)
}



