################################################################################
# Description: dbplyr supporting functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## Append database name to schema name ------------------------------------ ----
#' Append database and schema name
#' 
#' Some database connections can be used to access more than one database on the
#' same server. This requires that we explicitly include the database and schema
#' names in the connection.
#' 
#' This function joins the two together so they can be passed to 
#' `dbplyr::in_schema`.
#' 
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' 
#' @return appended database and schema name
#' 
#' @export
db_schema = function(db, schema) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  
  if(db == "" & schema == ""){
    stop("no db or schema")
  }
  
  if(db == "" & schema != ""){
    warn_if_missing_delimiters("[db]", schema, "[table]")
    return(glue::glue("{schema}"))
  }
  
  if(db != "" & schema == ""){
    warn_if_missing_delimiters(db, "[schema]", "[table]")
    return(glue::glue("{db}"))
  }
  
  warn_if_missing_delimiters(db, schema, "[table]")
  return(glue::glue("{db}.{schema}"))
}

## Delimited checks ------------------------------------------------------- ----
#' Check for delimiters and special characters
#' 
#' For clarity it is recommended that db, schema, and table names
#' be delimited using square brackets: "[]".
#' 
#' This function provides a standardized warning for missing delimiters to use
#' internally. It also checks for special characters in all three.
#' 
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server.
#' @param schema the name of the schema containing the Table or View.
#' @param tbl_name the name of the Table or View in SQL.
#' 
warn_if_missing_delimiters = function(db, schema, tbl_name) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  
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

## Check table or view exists in database --------------------------------- ----
#' Check Table or View exists in the database
#' 
#' Returns TRUE if table or view exists in the database. Used in place of
#' `DBI::dbExistsTable` and `tbl %in% DBI::dbListTables(db_con)`
#' because these approaches do not always handle schema consistently.
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
#' @return T/F whether the Table or View exists
#' 
#' @export
table_or_view_exists_in_db = function(db_connection, db = "[]", schema = "[]", tbl_name) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  
  # check input
  warn_if_missing_delimiters(db, schema, tbl_name)
  tbl_name_clean = remove_delimiters(tbl_name, "[]")
  
  # query structures
  sqlite_query = glue::glue(
    "SELECT EXISTS (\n",
    "SELECT name FROM sqlite_master WHERE name='{tbl_name_clean}'\n",
    ") AS ans"
  )
  
  sqlserver_query = glue::glue(
    "IF OBJECT_ID('{db}.{schema}.{tbl_name}', 'U') IS NOT NULL\n",
    "OR OBJECT_ID('{db}.{schema}.{tbl_name}', 'V') IS NOT NULL\n",
    "SELECT 1 AS ans ELSE SELECT 0 AS ans"
  )
  
  # query selection
  query = dplyr::case_when(
    any(grepl("SQLite", class(db_connection))) ~ sqlite_query,
    any(grepl("SQL Server", class(db_connection))) ~ sqlserver_query
  )
  
  exists = DBI::dbGetQuery(db_connection, query)
  return(unlist(exists, use.names = FALSE) == 1)
}

## Check table has required columns --------------------------------------- ----
#' Confirm table contains the required columns
#' 
#' Returns TRUE if the table contains all the required columns.
#' If only = TRUE, returns TRUE if the table ONLY contains all
#' the required columns.
#' 
#' Checks column names only, does not consider contents.
#' 
#' @param tbl_to_check a data frame. Works with local and remote tables.
#' @param required_columns an array of character containing the names of the
#' required columns.
#' @param only if `only = TRUE`, checks that the table ONLY contains all the
#' required columns. Additional columns will cause the function to return FALSE.
#' Defaults to FALSE.
#' 
#' @return T/F whether the (remote) table contains all the required columns.
#' @export
#' 
table_contains_required_columns = function(tbl_to_check, required_columns, only = FALSE) {
  stopifnot(dplyr::is.tbl(tbl_to_check) | is.data.frame(tbl_to_check))
  stopifnot(is.character(required_columns))
  stopifnot(is.logical(only))
  
  # column names of table
  table_column_names = colnames(tbl_to_check)
  
  # required columns in table
  correct = all(required_columns %in% table_column_names)
  # only required columns in table
  if (only) {
    correct = correct & all(table_column_names %in% required_columns)
  }
  
  return(correct)
}

## Save SQL queries to files ---------------------------------------------- ----
#' Save SQL queries to files
#' 
#' For transparency and ease of debugging, we make a practice of saving to a
#' temporary folder all the SQL queries that write or change data on the server
#' (but not those that only fetch data).
#' 
#' This function provides a standardized way to save SQL code. As these scripts
#' are primarily intended to support debugging, they can be deleted without
#' concern.
#' 
#' @param query the text of the query to save. This may be generated using
#' `dplyr::show_query` where required.
#' @param desc a description of the query. Use to name the file.
#' @param query_path The path to save the query. If provided will attempt to
#' save a copy of the query to the provided folder. Skips of no path.
#' Warns if unable to write. Creates directory if required.
#' 
#' @return The location and name of the saved file. If permissions do not exist
#' to save the file, the name and location of the attempted file.
#' 
#' @export
save_to_sql_script = function(query, desc, query_path = NA) {
  # exit early
  if(is.na(query_path)){
    return(invisible())
  }
  stopifnot(dbplyr::is.sql(query) | is.character(query))
  stopifnot(is.character(desc))
  stopifnot(is.character(query_path))
  
  # tiny delay ensures no two files writes can have the same time-stamp
  Sys.sleep(0.1)
  
  # time stamp includes milliseconds
  clean_time = gsub("[.:]", "-", format(Sys.time(), "%Y-%m-%d %H%M%OS3"))
  clean_name = gsub("[. :]", "_", desc)
  file_name = glue::glue("{clean_time} {clean_name}.sql")
  
  tryCatch(
    # try to write file
    {
      # create directory if required
      if (!dir.exists(query_path)) { dir.create(query_path) }
      writeLines(as.character(query), file.path(query_path, file_name))
      return(file.path(query_path, file_name))
    },
    # if error > display as warning
    error = function(e){
      msg = glue::glue("Error while saving query text:\n{e}")
      warning(msg)
    },
    # if warning > display
    warning = function(w){
      msg = glue::glue("Waring while saving query text:\n{w}")
      warning(msg)
    }
  )
  
  return(invisible())
}
