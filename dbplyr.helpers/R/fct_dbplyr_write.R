################################################################################
# Description: dbplyr write to database functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## Create new table in the database --------------------------------------- ----
#' Create new database table
#' 
#' Creates a new empty table in the database, overwriting an existing table if
#' instructed. Requires create table permissions.
#' 
#' Often used prior to `append_database_table`. Optional to use it prior to
#' `write_to_database`.
#' 
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param tbl_name the name of the Table in SQL. Warns is not delimited
#' in square brackets.
#' @param named_list_of_columns a named array, where the names are the column
#' names and the contents are the column data types. See details below.
#' @param OVERWRITE T/F should any existing table of the same name be deleted
#' first? Defaults to FALSE, and will error if a table with the same name
#' already exists.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#' 
#' @return the result from executing the command using `DBI::dbExecute`
#' (seldom used).
#' 
#' @details `named_list_of_columns` takes the format: `name = "sql type"`
#' For example:
#' ```
#' c(
#'   number_column_name = "[int] NOT NULL",
#'   date_column_name = "[date] NOT NULL",
#'   character_column_name = "[varchar](25) NULL"
#' )
#' ```
#' 
#' @export
create_table = function(db_connection, db = "[]", schema = "[]", tbl_name, named_list_of_columns, OVERWRITE = FALSE, query_path = NA) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  warn_if_missing_delimiters(db, schema, tbl_name)
  stopifnot(length(named_list_of_columns) >= 1)
  
  stopifnot(OVERWRITE %in% c(TRUE, FALSE))
  
  # remove table if it exists
  if (OVERWRITE){
    delete_table(db_connection, db, schema, tbl_name, query_path = query_path)
  }
  
  # SQL Server setup queries
  if(any(grepl("SQL Server", class(db_connection)))){
    DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_NULLS ON")))
    DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET QUOTED_IDENTIFIER ON")))
    DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_PADDING ON")))
  }
  
  # new table
  new_table = dplyr::case_when(
    any(grepl("SQLite", class(db_connection))) ~ glue::glue("{tbl_name}"),
    any(grepl("SQL Server", class(db_connection))) ~ glue::glue("{db}.{schema}.{tbl_name}")
  )
  
  # main SQL query
  sql_query = dbplyr::build_sql(
    con = db_connection,
    "CREATE TABLE ", dbplyr::sql(new_table), "(", "\n",
    dbplyr::sql(paste0("[", names(named_list_of_columns), "] ",
                       named_list_of_columns, collapse = ",\n")), "\n",
    ")"
  )
  
  # run query
  save_to_sql_script(sql_query, "create_table", query_path)
  result = DBI::dbExecute(db_connection, as.character(sql_query))
  
  # SQL Server post queries
  if(any(grepl("SQL Server", class(db_connection)))){
    DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_PADDING OFF")))
  }
  return(result)
}

## Append rows to an existing table --------------------------------------- ----
#' Append rows to an existing table
#' 
#' Given a table from a database connection, append it to an existing table
#' using the INSERT INTO ... (COLUMN NAMES) SELECT ... FROM ... pattern.
#' 
#' But source tables must exist and be database tables. If the receiving table
#' does not already exist, then you can create it with `create_table`. If
#' wanting to append a local R data frame, then you can first copy it to the
#' database using `copy_r_to_sql`.
#' 
#' @param table_to_append an existing remote table that contains the rows to
#' append.
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param tbl_name the name of the Table in SQL. Warns is not delimited
#' in square brackets.
#' @param list_of_columns a list of columns found in both tables. Determine
#' which columns from `table_to_append` are appended.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#' 
#' @return the result from executing the command using `DBI::dbExecute`
#' (seldom used).
#' 
#' A common error occurs when character strings in the appended table exceed the
#' length of the existing VARCHAR(n) limit in the receiving table. I.e. avoid
#' appending a 5 character string like 'ABCDE' into a VARCHAR(3) column.
#' 
#' @export
append_database_table = function(table_to_append, db_connection, db = "[]", schema = "[]", tbl_name, list_of_columns, query_path = NA) {
  stopifnot("tbl_sql" %in% class(table_to_append))
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  warn_if_missing_delimiters(db, schema, tbl_name)
  stopifnot(table_or_view_exists_in_db(db_connection, db, schema, tbl_name))
  stopifnot(length(list_of_columns) >= 1)
  stopifnot(is.character(list_of_columns))
  stopifnot(all(list_of_columns %in% colnames(table_to_append)))
  
  table_being_appended = create_access_point(db_connection, db, schema, tbl_name)
  stopifnot(all(list_of_columns %in% colnames(table_being_appended)))
  
  table_to_append = dplyr::ungroup(table_to_append)
  table_to_append = dplyr::select(table_to_append, dplyr::all_of(list_of_columns))
  
  sql_list_of_columns = paste0(
    dbplyr::escape(dbplyr::ident(list_of_columns), con = db_connection),
    collapse = ", "
  )
  
  # receiving table
  receiving_table = dplyr::case_when(
    any(grepl("SQLite", class(db_connection))) ~ glue::glue("{tbl_name}"),
    any(grepl("SQL Server", class(db_connection))) ~ glue::glue("{db}.{schema}.{tbl_name}")
  )
  
  query = glue::glue(
    "INSERT INTO {receiving_table} ({sql_list_of_columns})\n",
    "{dbplyr::sql_render(table_to_append)}"
  )
  
  # print(query)
  save_to_sql_script(query, "append_table", query_path)
  result = DBI::dbExecute(db_connection, as.character(query))
}

## Write to database ------------------------------------------------------ ----
#' Write database table back to database
#' 
#' In R it is straightforward, to load a csv file, undertake some manipulation
#' and then save out a new csv file. However, when working with databases, this
#' is not as common a pattern for analysis. Still we need the option to save
#' a specific table to the database.
#' 
#' Given a table from a database connection, writes to a new table using the
#' SELECT ... INTO ... FROM ... pattern.
#' 
#' Intended to be used in a Sandpit environment. Will error is user does not
#' have create table or insert into permissions.
#'
#' Does not write R tables into SQL. The `input_tbl` must be a remote data
#' frame. Use `copy_r_to_sql` if the input table is a local R data frame.
#' 
#' @param input_tbl an existing remote table that defines/describes the new
#' table to save
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param tbl_name the name of the Table in SQL. Warns is not delimited
#' in square brackets.
#' @param OVERWRITE T/F should any existing table of the same name be deleted
#' first? Defaults to FALSE, and will error if a table with the same name
#' already exists.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#' 
#' @return a connection to the new table (using `create_access_point`)
#' 
#' @export

write_to_database = function(input_tbl, db_connection, db = "[]", schema = "[]", tbl_name, OVERWRITE = FALSE, query_path = NA) {
  stopifnot("tbl_sql" %in% class(input_tbl))
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  warn_if_missing_delimiters(db, schema, tbl_name)
  
  # remove table if it exists
  if (OVERWRITE){
    delete_table(db_connection, db, schema, tbl_name, query_path = query_path)
  }
  
  # connection
  tbl_connection = input_tbl$src$con
  # setup
  from_id = dbplyr::ident("subquery")
  
  # SQL query
  sql_query = dplyr::case_when(
    any(grepl("SQLite", class(db_connection))) ~ glue::glue(
      "CREATE TABLE {tbl_name} AS \n",
      "SELECT *\n",
      "FROM (\n",
      dbplyr::sql_render(input_tbl),
      "\n) {from_id}"
    ),
    any(grepl("SQL Server", class(db_connection))) ~ glue::glue(
      "SELECT *\n",
      "INTO {db}.{schema}.{tbl_name}\n",
      "FROM (\n",
      dbplyr::sql_render(input_tbl),
      "\n) {from_id}"
    )
  )
  
  # run query
  save_to_sql_script(sql_query, "write_to_database", query_path)
  result = DBI::dbExecute(db_connection, as.character(sql_query))
  
  # load and return new table
  return(create_access_point(db_connection, db, schema, tbl_name))
}

## write interim table for reuse ------------------------------------------ ----
#' Write remote table for reuse
#' 
#' During long or complex manipulations we may wish to write the current state
#' of a remote database table back to disk before we resume manipulating it.
#' 
#' `write_for_reuse` has been designed for this purpose. It can be inserted
#' into the middle of a sequence of piped dplyr commands to force a write to
#' disk. It only works for SQL Server.
#' 
#' Existing tables with the same name are automatically overwritten. For a more
#' cautious approach, use `write_to_database` followed by
#' `create_nonclustered_index`.
#' 
#' @param tbl_to_save an existing remote table that defines/describes the new
#' table to save
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param tbl_name the name of the Table in SQL. Warns is not delimited
#' in square brackets.
#' @param index_columns an optional array containing the names of columns for
#' to index by. Will error if columns are not found in table.
#' @param print_off T/F should progress be printed to console. Defaults to TRUE.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#' 
#' @return a connection to the new table (using `create_access_point`)
#' 
#' @export
write_for_reuse = function(tbl_to_save, db_connection, db, schema, tbl_name, index_columns = NA, print_off = TRUE, query_path = NA) {
  stopifnot("tbl_sql" %in% class(tbl_to_save))
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  warn_if_missing_delimiters(db, schema, tbl_name)
  connection_is_sql_server = any(grepl("SQL Server", class(db_connection)))
  stopifnot(connection_is_sql_server)
  
  run_time_inform_user("writing temporary table", context = "all", print_off = print_off)
  saved_table = write_to_database(tbl_to_save, db_connection, db, schema, tbl_name, OVERWRITE = TRUE, query_path)
  run_time_inform_user("completed write", context = "all", print_off = print_off)
  
  if (length(index_columns) > 1 | !is.na(index_columns[1])) {
    result = create_nonclustered_index(db_connection, db, schema, tbl_name, index_columns, query_path)
    run_time_inform_user("added index", context = "all", print_off = print_off)
  }
  
  return(saved_table)
}

## Copy R table to SQL ---------------------------------------------------- ----
#' Copy local R table to remote database
#' 
#' There are several different ways to copy local R data frames onto a server.
#' As the inbuilt dbplyr `copy_to` function did not work with our setup (and
#' worse, caused errors/locking for other users) we implemented this apporach
#' using the DBI package.
#' 
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param sql_table_name the name of the Table in SQL. Warns is not delimited
#' in square brackets.
#' @param r_table_name a data frame in R to be copied to the database.
#' @param OVERWRITE T/F should any existing table of the same name be deleted
#' first? Defaults to FALSE, and will error if a table with the same name
#' already exists.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#' 
#' @return a connection to the new table (using `create_access_point`)
#' 
#' @export
copy_r_to_sql = function(db_connection, db = "[]", schema = "[]", sql_table_name, r_table_name, OVERWRITE = FALSE, query_path = NA) {
  stopifnot(is.data.frame(r_table_name))
  stopifnot("tbl_sql" %not_in% class(r_table_name))
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(sql_table_name))
  stopifnot(OVERWRITE %in% c(TRUE, FALSE))
  warn_if_missing_delimiters(db, schema, sql_table_name)
  
  # remove if overwrite
  if (OVERWRITE) {
    delete_table(db_connection, db, schema, sql_table_name, query_path = query_path)
  }
  
  # ID options
  db_delim = remove_delimiters(db, "[]")
  schema_delim = remove_delimiters(schema, "[]")
  
  if(nchar(db_delim) > 0 & nchar(schema_delim) > 0) {
    this_ID = DBI::Id(
      catalog = remove_delimiters(db, "[]"),
      schema = remove_delimiters(schema, "[]"),
      table = remove_delimiters(sql_table_name, "[]")
    )
  } else if(nchar(db_delim) > 0 & nchar(schema_delim) == 0) {
    this_ID = DBI::Id(
      catalog = remove_delimiters(db, "[]"),
      table = remove_delimiters(sql_table_name, "[]")
    )
  } else if(nchar(db_delim) == 0 & nchar(schema_delim) > 0) {
    this_ID = DBI::Id(
      schema = remove_delimiters(schema, "[]"),
      table = remove_delimiters(sql_table_name, "[]")
    )
  } else {
    this_ID = DBI::Id(
      table = remove_delimiters(sql_table_name, "[]")
    )
  }
  
  # copy data
  suppressMessages( # mutes translation message
    DBI::dbWriteTable(db_connection, this_ID, r_table_name)
  )
  
  r_table_name = create_access_point(db_connection, db, schema, sql_table_name)
  return(r_table_name)
}

## Create views ----------------------------------------------------------- ----
#' Create a View from an existing remote table
#' 
#' The view equivalent of `write_to_database`. Given a table from a database
#' connection, defines a new View with this definition.
#' 
#' The original table must come from an SQL connection. Can only create views on
#' the database specified by the connection. Requires permission to create views
#' on this database.
#' 
#' @param tbl_name an existing remote table that defines/describes the new view
#' @param db_connection the open connection to close.
#' Often created using `create_database_connection`.
#' @param db the name of the database. Can be different from the database used
#' when creating `db_connection` but must be on the same server. Warns if not
#' delimited in square brackets.
#' @param schema the name of the schema containing the Table or View. Warns if
#' not delimited in square brackets.
#' @param view_name the name of the Table or View in SQL. Warns is not delimited
#' in square brackets.
#' @param OVERWRITE T/F should any existing view of the same name be deleted
#' first? Defaults to FALSE, and will error if a view with the same name already
#' exists.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#' 
#' @return a connection to the new view (using `create_access_point`)
#' 
#' @export
create_view = function(tbl_name, db_connection, db = "[]", schema = "[]", view_name, OVERWRITE = FALSE, query_path = NA) {
  stopifnot("tbl_sql" %in% class(tbl_name))
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(view_name))
  stopifnot(OVERWRITE %in% c(TRUE, FALSE))
  warn_if_missing_delimiters(db, schema, view_name)
  
  # remove view if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, view_name, mode = "view", query_path)
  
  # location
  if(any(grepl("SQLite", class(db_connection)))) {
    view_delim = remove_delimiters(view_name, "[]")
    new_view_name = glue::glue("{view_delim}")
  } else {
    new_view_name = glue::glue("{schema}.{view_name}")
  }
  
  # SQL query
  sql_query = glue::glue(
    "CREATE VIEW {new_view_name} AS\n",
    "{dbplyr::sql_render(tbl_name)}\n"
  )
  
  # run query
  save_to_sql_script(sql_query, "create_view", query_path)
  result = DBI::dbExecute(db_connection, as.character(sql_query))
  
  # load and return new table
  return(create_access_point(db_connection, db, schema, view_name))
}
