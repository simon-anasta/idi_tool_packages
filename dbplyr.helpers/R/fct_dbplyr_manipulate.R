################################################################################
# Description: dbplyr manipulate database tables
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## Delete (drop) tables and views from SQL -------------------------------- ----
#' Drop tables and views from SQL
#' 
#' Delete/drop tables and views. Includes a check for existence, so will not
#' error if table or view does not exist.
#' 
#' Can only drop views if `db_connection` contains the name of the database
#' where the Views are stored.
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
#' @param mode either "table" or "view". Determines whether a table or view is
#' dropped.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#'
#' @return the result from executing the command using `DBI::dbExecute`
#' (seldom used).
#' 
#' @export
delete_table = function(db_connection, db = "[]", schema = "[]", tbl_name, mode = "table", query_path = NA) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  
  warn_if_missing_delimiters(db, schema, tbl_name)
  mode = tolower(mode)
  stopifnot(mode %in% c("view", "table"))
  
  # type
  if (mode == "table"){ code = "U"}
  if (mode == "view"){ code = "V"}
  
  # all outputs must be the same type (as.character) not (glue/character)
  view_output = as.character(glue::glue("{schema}.{tbl_name}"))
  full_output = as.character(glue::glue("{db}.{schema}.{tbl_name}"))
  
  table_view_name = dplyr::case_when(
    any(grepl("SQLite", class(db_connection))) ~ tbl_name,
    any(grepl("SQL Server", class(db_connection))) && mode == "view" ~ view_output,
    TRUE ~ full_output
  )
  
  # remove table if it exists
  removal_query = glue::glue("DROP {toupper(mode)} IF EXISTS {table_view_name};")
  save_to_sql_script(removal_query, paste0("delete_", mode), query_path)
  result = DBI::dbExecute(db_connection, as.character(removal_query))
}

## Add non-clustered index to a table ------------------------------------- ----
#' Add non-clustered index
#' 
#' Adds non-clustered indexes to an SQL Server table. Errors for non-SQL Server
#' tables.
#' 
#' Create a non-clustered index to improve performance of joins.
#' Unlike clustered indexes, multiple non-clustered indexes can be created.
#' 
#' Clustered indexes tend to perform faster than non-clustered indexes.
#' Permanent high reuse tables may benefit from clustered indexes. But as most
#' researcher-created tables are used a limited number of times, they do not
#' justify the additional overhead of creating a clustered index. Hence 
#' non-clustered indexes are recommended.
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
#' @param index_columns an array containing the names of columns for the
#' index. Will error if columns are not found in table.
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#'
#' @return the result from executing the command using `DBI::dbExecute`
#' (seldom used).
#' 
#' @export
create_nonclustered_index = function(db_connection, db, schema, tbl_name, index_columns, query_path = NA) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  stopifnot(is.character(index_columns))
  connection_is_sql_server = any(grepl("SQL Server", class(db_connection)))
  stopifnot(connection_is_sql_server)
  
  warn_if_missing_delimiters(db, schema, tbl_name)
  # table in connection
  stopifnot(table_or_view_exists_in_db(db_connection, db, schema, tbl_name))

  # columns are in table
  table_to_index = create_access_point(db_connection, db, schema, tbl_name)
  stopifnot(all(index_columns %in% colnames(table_to_index)))
  
  index_columns = sapply(index_columns, add_delimiters, delimiter = "[]")
  index_columns = paste0(index_columns, collapse = ", ")
  
  query = glue::glue("CREATE NONCLUSTERED INDEX my_index_name ON {db}.{schema}.{tbl_name} ({index_columns})")
  
  save_to_sql_script(query, "add_nonclustered_index", query_path)
  result = DBI::dbExecute(db_connection, as.character(query))
}

## Compress a table ------------------------------------------------------- ----
#' Compress a table
#' 
#' Compacts an SQL Server table. Errors for non-SQL Server tables.
#' 
#' Large SQL tables can be compressed to reduce the space they take up.
#' We have observe this reduce storage space of tables by 50-75% in many cases.
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
#' @param query_path If provided will attempt to save a copy of the SQL code
#' sent to/executed on the database to the provided folder. Save occurs before
#' execution, hence useful for debugging.
#'
#' @return the result from executing the command using `DBI::dbExecute`
#' (seldom used).
#' 
#' @export
compress_table = function(db_connection, db, schema, tbl_name, query_path = NA) {
  stopifnot(is.character(db))
  stopifnot(is.character(schema))
  stopifnot(is.character(tbl_name))
  connection_is_sql_server = any(grepl("SQL Server", class(db_connection)))
  stopifnot(connection_is_sql_server)
  
  warn_if_missing_delimiters(db, schema, tbl_name)
  stopifnot(table_or_view_exists_in_db(db_connection, db, schema, tbl_name))
  
  # SQL query
  sql_query = glue::glue("ALTER TABLE {db}.{schema}.{tbl_name} REBUILD PARTITION = ALL WITH (DATA_COMPRESSION = PAGE)")
  
  # run query
  save_to_sql_script(sql_query, "compress_table", query_path)
  result = DBI::dbExecute(db_connection, as.character(sql_query))
}

## Union all -------------------------------------------------------------- ----
#' SQL Server Union all
#' 
#' Appends two tables, requires that both tables have identical column names.
#' 
#' @param table_a a remote (database) table
#' @param table_b another remote (database) table
#' @param list_of_columns a list of columns found in both tables to be included
#' in the output table.
#' 
#' @return a remote table containing the columns listed where the rows are
#' the combination of both tables.
#' 
#' @export
union_all = function(table_a, table_b, list_of_columns) {
  stopifnot("tbl_sql" %in% class(table_a))
  stopifnot("tbl_sql" %in% class(table_b))
  stopifnot(length(list_of_columns) >= 1)
  stopifnot(is.character(list_of_columns))
  
  # connection
  db_connection = table_a$src$con
  
  table_a = dplyr::ungroup(table_a)
  table_a = dplyr::select(table_a, dplyr::all_of(list_of_columns))
  
  table_b = dplyr::ungroup(table_b)
  table_b = dplyr::select(table_b, dplyr::all_of(list_of_columns))
  
  sql_query = dbplyr::build_sql(
    con = db_connection,
    "SELECT * FROM (\n",
    dbplyr::sql_render(table_a),
    ") AS sub_a\n",
    "\nUNION ALL\n",
    "SELECT * FROM (\n",
    dbplyr::sql_render(table_b),
    ") AS sub_b",
  )
  return(dplyr::tbl(db_connection, dbplyr::sql(sql_query)))
}

## Pivot long-thin table to rectangular table ----------------------------- ----
#' Pivot wider
#' 
#' Pivots long-thin tables to rectangular, taking a label-value pair of columns
#' and transforming table so labels that were cell values become column names.
#' 
#' SQL equivalent of tidyr::spread
#'
#' Note that this tends to be an expensive operation. So best not to include as
#' part of a long or complex chains of commands.
#'
#' @param input_tbl a remote (database) table
#' @param label_column the name of the column containing the labels
#' @param value_column the name of the column containing the values
#' @param aggregator where the labels are not unique, how should the multiple
#' values be resolved. Defaults to "SUM", but also works with any SQL
#' aggregation function such as "MAX", "MIN".
#'
#' @return a remote (database) table that has been pivoted wider.
#' 
#' @export
pivot_table = function(input_tbl, label_column, value_column, aggregator = "sum") {
  # checks
  stopifnot("tbl_sql" %in% class(input_tbl))
  stopifnot(label_column %in% colnames(input_tbl))
  stopifnot(value_column %in% colnames(input_tbl))
  stopifnot(length(label_column) == 1)
  stopifnot(length(value_column) == 1)
  stopifnot(is.character(aggregator))
  no_special_characters(aggregator)
  
  # pivot components
  non_pivot_columns = colnames(input_tbl)
  non_pivot_columns = non_pivot_columns[non_pivot_columns %not_in% c(label_column, value_column)]
  
  pivot_columns = dplyr::select(input_tbl, dplyr::all_of(label_column))
  pivot_columns = dplyr::filter(pivot_columns, !is.na(!!rlang::sym(label_column)))
  pivot_columns = dplyr::distinct(pivot_columns)
  pivot_columns = dplyr::collect(pivot_columns)
  pivot_columns = unlist(pivot_columns, use.names = FALSE)
  pivot_columns = sort(pivot_columns)
  
  # expression creation
  #
  # extra handling of `na.rm = TRUE` required as:
  # - without: WARNING "na always removed"
  # - with: translation error
  if(any(grepl("SQL Server", class(input_tbl)))){
    expressions = glue::glue("{aggregator}(ifelse({label_column} == '{pivot_columns}', {value_column}, 0))")
  } else {
    expressions = glue::glue("{aggregator}(ifelse({label_column} == '{pivot_columns}', {value_column}, 0), na.rm = TRUE)")
  }
  expressions = as.list(rlang::parse_exprs(expressions))
  names(expressions) = pivot_columns
  
  output = dplyr::group_by(input_tbl, !!!rlang::syms(non_pivot_columns))
  output = dplyr::summarise(output, !!!expressions)
  output = dplyr::ungroup(output)
  
  return(output)
}

## collapse indicator columns --------------------------------------------- ----
#' Combine indicator columns to single column
#' 
#' Many tables have mutually exclusive indicator columns. This function provides
#' a compact way to combine these columns to a single column where the original
#' columns all share the same prefix.
#' 
#' For example: `sex_male = c("y", "n", "n"), sex_female = c("n", "y", "y")`
#' Becomes: `sex = c("male", "female", "female")`
#' 
#' @param input_tbl a table or data frame. Works with local and remote tables.
#' @param prefix the prefix shared between all columns to combine
#' @param yes_values values that indicate the variable is true/active. Case-
#' sensitive. The result from combining different data types
#' (for example `c(1, "y")`)  will depend on implicit conversions.
#' @param label an (optional) title for the new column. Defaults to the prefix.
#'
#' @return a remote (database) table where the multiple columns with the given
#' prefix have been replaced by a single categorical column.
#' 
#' Note that where columns are not mutually exclusive, this function should not
#' will remote duplication invisibly, only preserving a single value.
#'
#' For example: suppose people can have multiple eye colors:
#' `eye_blue = c(1,1,0), eye_brown = c(1,1,1), eye_green = c(0,1,1)`
#' Give output: `eye = c("blue","blue","brown")`
#' 
#' @export
collapse_indicator_columns = function(input_tbl, prefix, yes_values, label = prefix) {
  # validate input
  stopifnot(is.data.frame(input_tbl) | dplyr::is.tbl(input_tbl))
  stopifnot(is.character(prefix))
  stopifnot(is.character(label))
  stopifnot(length(yes_values) >= 1)
  
  # column names
  colnames_list = colnames(input_tbl)
  matches = grepl(paste0("^", prefix), colnames_list)
  colnames_match = colnames_list[matches]
  colnames_nonmatch = colnames_list[!matches]
  
  colnames_match = sort(colnames_match)
  
  suffix_list = sub(prefix, "", colnames_match)
  
  # warn if no matches
  if (length(colnames_match) == 0) {
    warning("prefix matches no column names")
    return(input_tbl)
  }
  
  # expression creation
  yes_text = paste0("c(", paste0(yes_values, collapse = ","), ")")
  expressions = rlang::parse_exprs(glue::glue("`{colnames_match}` %in% {yes_text} ~ '{suffix_list}'"))
  `:=` = rlang::`:=`
  
  # collapse
  output_tbl = dplyr::mutate(
    input_tbl,
    !!rlang::sym(label) := dplyr::case_when( !!!expressions )
  )
  
  output_tbl = dplyr::select(
    output_tbl,
    dplyr::all_of(c(colnames_nonmatch, label))
  )
  return(output_tbl)
}
