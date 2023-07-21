################################################################################
# Description: dbplyr write to database functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Inform user with time stamped measure ---------------------------------- ----


## write out interum table for reuse --------------------------------------------------------------
#'
#' Takes an R object (tbl_to_save) that is an SQL table that has been
#' manipulated/transformed and saves it as a new table in SQL.
#' Returns a connection to the new table.
#'
#' For complex or extended analyses, this is recommended as it reduces
#' the complexity of the underlying SQL query that defines the manipulated
#' table.
#'
write_for_reuse <- function(db_connection, db, schema, tbl_name, tbl_to_save, index_columns = NA) {
  # check input
  warn_if_missing_delimiters(db, schema, tbl_name)
  
  run_time_inform_user("writing temporary table", context = "all")
  saved_table <- write_to_database(tbl_to_save, db_connection, db, schema, tbl_name, OVERWRITE = TRUE)
  run_time_inform_user("completed write", context = "all")
  
  if (length(index_columns) > 1 | !is.na(index_columns[1])) {
    result <- create_nonclustered_index(db_connection, db, schema, tbl_name, index_columns)
    run_time_inform_user("added index", context = "all")
  }
  
  return(saved_table)
}

## Write to database ------------------------------------------------------------------------------
#' Returning connection to the new table
#'
#' Given a table from a database connection, writes to a new table using the
#' SELECT ... INTO ... FROM ... pattern.
#'
#' Original table must come from an SQL connection.
#' Does not write R tables into SQL. Use copy_r_to_sql for this.
#'
#' E.g. the following works
#'       my_table <- create_access_point(....)
#'       write_to_database(my_table, ...)
#' But this will not work
#'       my_table <- data.frame(x = 1:10, y = 1:10)
#'       write_to_database(my_table, ...)
#'
write_to_database <- function(input_tbl, db_connection, db, schema, tbl_name, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  # checks
  assert("tbl_sql" %in% class(input_tbl), "input table must originate from sql connection")
  
  # remove table if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, tbl_name)
  
  # connection
  tbl_connection <- input_tbl$src$con
  # setup
  from_id <- dbplyr::ident(paste0("long", floor(runif(1, 1000000, 9999999))))
  
  # SQL query
  sql_query <- glue::glue(
    "SELECT *\n",
    "INTO {db}.{schema}.{tbl_name}\n",
    "FROM (\n",
    dbplyr::sql_render(input_tbl),
    "\n) {from_id}"
  )
  
  # run query
  save_to_sql(sql_query, "write_to_database")
  result <- dbExecute(db_connection, as.character(sql_query))
  
  # load and return new table
  create_access_point(db_connection, db, schema, tbl_name)
}

## Create views -----------------------------------------------------------------------------------
#' Returning connection to the new view
#'
#' The view equivalent of write_to_database. Given a table from a database
#' connection, defines a new view with this definition.
#'
#' The original table must come from an SQL connection.
#' Can only create views if the input connection is to the database containing
#' the views.
#'
create_view <- function(tbl_name, db_connection, db, schema, view_name, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, view_name)
  # checks
  assert("tbl_sql" %in% class(tbl_name), "input table must originate from sql connection")
  
  # remove view if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, view_name, mode = "view")
  
  # SQL query
  sql_query <- glue::glue(
    "CREATE VIEW {schema}.{view_name} AS\n",
    "{dbplyr::sql_render(tbl_name)}\n"
  )
  
  # run query
  save_to_sql(sql_query, "create_view")
  result <- DBI::dbExecute(db_connection, as.character(sql_query))
  
  # load and return new table
  create_access_point(db_connection, db, schema, view_name)
}

## Copy R table to SQL ----------------------------------------------------------------------------
#'
#' The inbuilt dbplyr copy_to function does not appear to work in our set-up.
#' Worse, it caused errors/locking, preventing other users who are using the
#' same connection method until their R session is restarted.
#'
#' Hence we implement a established work around using the DBI package.
#' This is a revision of the original function (in previous versions),
#' and uses different functionality to simplify the process.
#'
copy_r_to_sql <- function(db_connection, db, schema, sql_table_name, r_table_name, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, sql_table_name)
  
  # remove if overwrite
  if (OVERWRITE) delete_table(db_connection, db, schema, sql_table_name)
  
  suppressMessages( # mutes translation message
    DBI::dbWriteTable(
      db_connection,
      DBI::Id(
        catalog = remove_delimiters(db, "[]"),
        schema = remove_delimiters(schema, "[]"),
        table = remove_delimiters(sql_table_name, "[]")
      ),
      r_table_name
    )
  )
  
  r_table_name <- create_access_point(db_connection, db, schema, sql_table_name)
}

## Create new table in the database ---------------------------------------------------------------
#'
#' Creates an empty table in the database, overwriting an existing copy if
#' instructed. Main use is to setup an empyt table that results can be iteratively
#' appended to.
#'
#' Not necessary if creating a table directly from an existing table.
#' Use write_to_database for this.
#'
#' named_list_of_columns takes the format name = "sql type"
#' For example:
#' list_of_columns <- (number = "[int] NOT NULL",
#'                     date = "[date] NOT NULL",
#'                     character = "[varchar](25) NULL")
#'
create_table <- function(db_connection, db, schema, tbl_name, named_list_of_columns, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  
  # remove table if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, tbl_name)
  
  # setup queries
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_NULLS ON")))
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET QUOTED_IDENTIFIER ON")))
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_PADDING ON")))
  
  # main SQL query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "CREATE TABLE ",
    dbplyr::sql(db), ".",
    dbplyr::sql(schema), ".",
    dbplyr::sql(tbl_name), "(", "\n",
    dbplyr::sql(paste0("[", names(named_list_of_columns), "] ",
                       named_list_of_columns, collapse = ",\n")), "\n",
    ") ON [PRIMARY]"
  )
  
  # run query
  save_to_sql(sql_query, "create_table")
  result <- DBI::dbExecute(db_connection, as.character(sql_query))
  # post queries
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_PADDING OFF")))
  return(result)
}








## Append rows to an existing table ---------------------------------------------------------------
#'
#' Given a table from a database connection, append it to an existing table
#' using the INSERT INTO ... (COLUMN NAMES) SELECT ... FROM ... pattern.
#'
#' Like write_to_database, the original table must come from an SQL connection.
#' Does not append R tables onto SQL tables. You first need to write the R table
#' into SQL.
#'
#' A common error occurs when character strings in the appended table exceed the
#' length of the existing varchar(n) limit in the original dataset.
#' E.g. appending the string 'ABCDE' into a varchar(3) column will error.
#'
append_database_table <- function(db_connection, db, schema, tbl_name, list_of_columns, table_to_append) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  assert("tbl_sql" %in% class(table_to_append), "table to append must originate from sql connection")
  # table in connection
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  # tables contain list of columns
  assert(
    all(list_of_columns %in% colnames(table_to_append)),
    "table to append does not have required columns"
  )
  assert(
    all(list_of_columns %in% colnames(create_access_point(db_connection, db, schema, tbl_name))),
    "database table does not have the required columns"
  )
  
  table_to_append <- table_to_append %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(list_of_columns))
  
  sql_list_of_columns <- paste0(dbplyr::escape(dbplyr::ident(list_of_columns), con = db_connection), collapse = ", ")
  
  query <- glue::glue("INSERT INTO {db}.{schema}.{tbl_name} ({sql_list_of_columns})\n {dbplyr::sql_render(table_to_append)}")
  
  # print(query)
  save_to_sql(query, "append_table")
  result <- DBI::dbExecute(db_connection, as.character(query))
}

