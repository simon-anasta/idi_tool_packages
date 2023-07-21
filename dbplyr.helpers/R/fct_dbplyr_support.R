################################################################################
# Description: dbplyr supporting functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Inform user with time stamped measure ---------------------------------- ----

## Append database name to schema name ------------------------------------------------------------
#'
db_schema <- function(db, schema) {
  assert(is_delimited(db, "[]"), "database not correctly delimited")
  assert(is_delimited(schema, "[]"), "schema not correctly delimited")
  
  return(glue::glue("{db}.{schema}"))
}

## Delimited checks -------------------------------------------------------------------------------
#'
#' For consistency of approach it is recommended that SQL db, scema and table names
#' be delimited using square brackets: "[]".
#'
#' This function provides a standardised warning for missing delimiters that can be
#' inserted at the start of every function.
#'
#' Also checks for special characters in all three
#'
warn_if_missing_delimiters <- function(db, schema, tbl_name) {
  if (!is_delimited(db, "[]")) {
    warning("db is not delimited, delimiting with [] is recommended")
  }
  if (!is_delimited(schema, "[]")) {
    warning("schema is not delimited, delimiting with [] is recommended")
  }
  if (!is_delimited(tbl_name, "[]")) {
    warning("tbl_name is not delimited, delimiting with [] is recommended")
  }
  
  no_special_characters(db)
  no_special_characters(schema)
  no_special_characters(tbl_name)
}

## Check table or view exists in database ---------------------------------------------------------
#'
#' Returns true is table or view exists in the database
#' Used in place of 'dbExistsTable' and 'tbl %in% DBI::dbListTables(db_con)'
#' Because these approaches do not always handle schema consistently.
#'
table_or_view_exists_in_db <- function(db_connection, db, schema, tbl_name) {
  # check input
  warn_if_missing_delimiters(db, schema, tbl_name)
  
  query <- glue::glue(
    "IF OBJECT_ID('{db}.{schema}.{tbl_name}', 'U') IS NOT NULL\n",
    "OR OBJECT_ID('{db}.{schema}.{tbl_name}', 'V') IS NOT NULL\n",
    "SELECT 1 AS ans ELSE SELECT 0 AS ans"
  )
  
  exists <- DBI::dbGetQuery(db_connection, query)
  return(unlist(exists, use.names = FALSE) == 1)
}

## Check table has required columns ---------------------------------------------------------------
#'
#' Returns TRUE if the table contains all the required columns.
#' If only = TRUE, returns TRUE if the table ONLY contains all
#' the required columns.
#'
#' Checks column names only, does not consider contents.
#'
table_contains_required_columns <- function(tbl_to_check, required_columns, only = FALSE) {
  
  # column names of table
  table_column_names <- colnames(tbl_to_check)
  
  # required columns in table
  correct <- all(required_columns %in% table_column_names)
  # only required columns in table
  if (only) {
    correct <- correct & all(table_column_names %in% required_columns)
  }
  
  return(correct)
}

## Save SQL queries to files ----------------------------------------------------------------------
#'
#' All the SQL queries that write or change data on the server (but not those that
#' only fetch data) in these utility functions save an SQL code file of the command
#' that was executed.
#'
#' These scripts are primarily intended to support debugging.
#' They can be deleted without concern.
#'
save_to_sql <- function(query, desc) {
  if (!dir.exists("./SQL tmp scripts")) {
    dir.create("./SQL tmp scripts")
  }
  
  Sys.sleep(0.1) # tiny delay ensures no two files writes can have the same time-stamp
  clean_time <- gsub("[.:]", "-", format(Sys.time(), "%Y-%m-%d %H%M%OS3")) # includes milliseconds now
  clean_name <- gsub("[. :]", "_", desc)
  
  file_name <- paste0("./SQL tmp scripts/", clean_time, " ", clean_name, ".sql")
  
  writeLines(as.character(query), file_name)
}



