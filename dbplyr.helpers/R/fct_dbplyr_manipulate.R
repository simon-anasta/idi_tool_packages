################################################################################
# Description: dbplyr manipulate database tables
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Inform user with time stamped measure ---------------------------------- ----




## Union all --------------------------------------------------------------------------------------
#'
#'  Provides UNION ALL functionality from SQL. Appends two tables.
#'
#' Requires a input tables to have identical column names. Provides as output
#' a single table the "union all" of all the input tables.
#'
union_all <- function(table_a, table_b, list_of_columns) {
  assert("tbl_sql" %in% class(table_a), "input table is not a remote table")
  assert("tbl_sql" %in% class(table_b), "input table is not a remote table")
  # connection
  db_connection <- table_a$src$con
  
  table_a <- table_a %>% dplyr::ungroup() %>% dplyr::select(all_of(list_of_columns))
  table_b <- table_b %>% dplyr::ungroup() %>% dplyr::select(all_of(list_of_columns))
  
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    dbplyr::sql_render(table_a),
    "\nUNION ALL\n",
    dbplyr::sql_render(table_b)
  )
  return(dplyr::tbl(db_connection, dbplyr::sql(sql_query)))
}

## Delete (drop) tables and views from SQL. -------------------------------------------------------
#'
#' Checks for existence of table/view prior to dropping.
#' Can only drop views if the input connection is to the database containing
#' the views.
#'
delete_table <- function(db_connection, db, schema, tbl_name, mode = "table") {
  warn_if_missing_delimiters(db, schema, tbl_name)
  mode <- tolower(mode)
  assert(mode %in% c("view", "table"), "mode must be in (view, table)")
  
  # type
  if (mode == "table") code <- "U"
  if (mode == "view") code <- "V"
  
  # remove database name if view mode
  maybe_db_schema <- db_schema(db, schema)
  if (mode == "view") {
    maybe_db_schema <- schema
  }
  
  # remove table if it exists
  removal_query <- glue::glue(
    "IF OBJECT_ID('{maybe_db_schema}.{tbl_name}', '{code}') IS NOT NULL\n",
    "DROP {toupper(mode)} {maybe_db_schema}.{tbl_name};"
  )
  save_to_sql(removal_query, paste0("delete_", mode))
  result <- DBI::dbExecute(db_connection, as.character(removal_query))
}

## Add nonclustered index to a table -----------------------------------------------------------------
#'
#' Create a nonclustered index to improve table performance.
#' Unlike clustered indexes, multiple nonclustered indexes can be created.
#' 
#' Permanent high reuse tables may benefit from clustered indexes. But most researcher-created
#' tables are used a limited number of times and hence do not justify the additional overhead
#' of creating a clustered index. Non-clustered indexes are recommended in these cases.
#'
create_nonclustered_index <- function(db_connection, db, schema, tbl_name, index_columns) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  # table in connection
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  # columns are in table
  assert(
    all(index_columns %in% colnames(create_access_point(db_connection, db, schema, tbl_name))),
    "database table does not have the required columns"
  )
  
  index_columns <- sapply(index_columns, add_delimiters, delimiter = "[]")
  index_columns <- paste0(index_columns, collapse = ", ")
  
  query <- glue::glue("CREATE NONCLUSTERED INDEX my_index_name ON {db}.{schema}.{tbl_name} ({index_columns})")
  
  # print(query)
  save_to_sql(query, "add_nonclustered_index")
  result <- DBI::dbExecute(db_connection, as.character(query))
}

## Compress a table -------------------------------------------------------------------------------
#'
#' Large SQL tables can be compressed to reduce the space they take up.
#'
compress_table <- function(db_connection, db, schema, tbl_name) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  
  # SQL query
  sql_query <- glue::glue("ALTER TABLE {db}.{schema}.{tbl_name} REBUILD PARTITION = ALL WITH (DATA_COMPRESSION = PAGE)")
  
  # run query
  save_to_sql(sql_query, "compress_table")
  result <- DBI::dbExecute(db_connection, as.character(sql_query))
}

## Pivot long-thin table to rectangular table -----------------------------------------------------
#'
#' Takes label-value pair of columns and transforms table
#' so labels that were column entries become column names.
#'
#' SQL equivalent of tidyr::spread
#'
#' Note that this tends to be an expensive operation.
#' So best not to include it as part of a long or complex
#' chain of commands.
#'
pivot_table <- function(input_tbl, label_column, value_column, aggregator = "SUM") {
  # checks
  assert("tbl_sql" %in% class(input_tbl), "input table must originate from sql connection")
  assert(label_column %in% colnames(input_tbl), sprintf("label column [%s] is not in table", label_column))
  assert(value_column %in% colnames(input_tbl), sprintf("value column [%s] is not in table", value_column))
  no_special_characters(aggregator)
  
  # connection
  db_connection <- input_tbl$src$con
  
  # pivot components
  non_pivot_columns <- colnames(input_tbl)
  non_pivot_columns <- non_pivot_columns[non_pivot_columns %not_in% c(label_column, value_column)]
  
  pivot_columns <- input_tbl %>%
    dplyr::select(!!sym(label_column)) %>%
    dplyr::filter(!is.na(!!sym(label_column))) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE) %>%
    sort()
  
  # check no special characters in new column labels
  # sapply(pivot_columns, no_special_characters)
  
  # query components
  from_id <- paste0("long", floor(runif(1, 1000000, 9999999)))
  pivot_id <- paste0("pvt", floor(runif(1, 1000000, 9999999)))
  non_pivot_columns <- paste0("[", non_pivot_columns, "]", collapse = ", ")
  pivot_columns <- paste0("[", pivot_columns, "]", collapse = ", ")
  value_column <- add_delimiters(value_column, "[]")
  
  # build SQL pivot query
  sql_query <- glue::glue(
    "SELECT {non_pivot_columns}, \n{pivot_columns}\n",
    "FROM (\n",
    "{dbplyr::sql_render(input_tbl)}\n",
    ") {from_id}\n",
    "PIVOT (\n",
    "{aggregator}({value_column}) FOR {label_column} IN \n({pivot_columns})\n",
    ") AS {pivot_id}"
  )
  
  #   dbplyr::build_sql(
  #   con = db_connection
  #   ,"SELECT ", dbplyr::sql(non_pivot_columns)
  #   ,"\n, ", dbplyr::sql(pivot_columns)
  #   ,"\nFROM (\n"
  #   ,dbplyr::sql_render(input_tbl)
  #   ,"\n) ", dbplyr::ident(from_id), "\n"
  #   ,"PIVOT (\n"
  #   ,dbplyr::sql(aggregator),"(", dbplyr::escape(dbplyr::ident(value_column), con = db_connection), ")"
  #   ,"\nFOR ", dbplyr::escape(dbplyr::ident(label_column), con = db_connection)
  #   , "\nIN (", dbplyr::sql(pivot_columns), ")"
  #   ,"\n) AS ", dbplyr::ident(pivot_id)
  # )
  
  return(dplyr::tbl(db_connection, dbplyr::sql(sql_query)))
}

## collapse indicator columns ---------------------------------------------------------------------
#'
#' Many tables have mutually exclusive indicator columns.
#' This function provides a compact way to combine these columns to a single column
#' where the original columns all share the same prefix.
#'
#' For example: sex_male = c("y", "n", "n"), sex_female = c("n", "y", "y")
#' Becomes: sex = c("male", "female", "female")
#'
#' Where columns are not mutually exclusive, this function should not be used as
#' only a single value will be preserved in the output column and any duplication
#' will be removed invisably.
#'
#' For example: eye_blue = c(1,1,0), eye_brown = c(1,1,1), eye_green = c(0,1,1)
#' Becomes eye = c("blue","blue","brown")
#'
collapse_indicator_columns <- function(input_tbl, prefix, yes_values, label = NA) {
  # validate input
  assert(is.data.frame(input_tbl) | is.tbl(input_tbl), "input table must be a dataframe")
  assert(is.character(prefix), "prefix must be a character string")
  assert(length(yes_values) >= 1, "at least one yes value must be provided")
  if (is.na(label)) {
    label <- prefix
  }
  assert(is.character(label), "label must be a character string")
  
  # column names
  colnames_list <- colnames(input_tbl)
  matches <- grepl(paste0("^", prefix), colnames_list)
  colnames_match <- colnames_list[matches]
  colnames_nonmatch <- colnames_list[!matches]
  
  colnames_match <- sort(colnames_match)
  
  suffix_list <- sub(prefix, "", colnames_match)
  
  # warn if no matches
  if (length(colnames_match) == 0) {
    warning("prefix matches no column names")
    return(input_tbl)
  }
  
  yes_text <- paste0("c(", paste0(yes_values, collapse = ","), ")")
  
  # collapse
  output_tbl <- input_tbl %>%
    mutate(!!sym(label) := case_when(
      !!!rlang::parse_exprs(paste0("`", colnames_match, "` %in% ", yes_text, " ~ '", suffix_list, "'"))
    )) %>%
    dplyr::select(all_of(c(colnames_nonmatch, label)))
}

