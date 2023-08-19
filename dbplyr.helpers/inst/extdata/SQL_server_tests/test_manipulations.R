################################################################################
# Description: SQL Server tests - manipulations
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Testing the following functions that handle basic SQL read/write
#    - union_all(table_a,table_b, list_of_columns)
#    - pivot_table(input_tbl, label_column, value_column, aggregator = "SUM")
#    - collapse_indicator_columns(input_tbl, prefix, yes_value, label = NA)
#
# Issues:
#
################################################################################

## union all -------------------------------------------------------------- ----

testthat::test_that("union row-binds", {
  # arrange
  head_table_name = "[test6829921]"
  tail_table_name = "[test4817588]"
  data(cars)
  head_cars = utils::head(cars)
  tail_cars = utils::tail(cars)
  
  # act - load and union
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  
  head_table = copy_r_to_sql(db_conn, table_db, our_schema, head_table_name, head_cars, query_path = query_path)
  tail_table = copy_r_to_sql(db_conn, table_db, our_schema, tail_table_name, tail_cars, query_path = query_path)
  unioned_table = union_all(head_table, tail_table, colnames(cars))
  unioned_table = dplyr::collect(unioned_table)
  
  rowbound_table = rbind(head_cars, tail_cars)
  
  # act - delete & tidy up
  delete_table(db_conn, table_db, our_schema, head_table_name, mode = "table", query_path = query_path)
  delete_table(db_conn, table_db, our_schema, tail_table_name, mode = "table", query_path = query_path)
  close_database_connection(db_conn)
  
  # assert
  testthat::expect_true(all_equal(unioned_table, rowbound_table, ignore_row_order = TRUE))
})

## pivot ------------------------------------------------------------------ ----

testthat::test_that("pivot replicates tidyr::spread", {
  # arrange
  table_name = "[test6597932]"
  
  in_data_table = data.frame(
    people = c("bob", "alice", "bob", "alice"),
    labels = c("age", "age", "height", "height"),
    values = c(10, 12, 150, 160),
    stringsAsFactors = FALSE
  )
  out_data_table = data.frame(
    people = c("bob", "alice"),
    age = c(10, 12),
    height = c(150, 160),
    stringsAsFactors = FALSE
  )
  
  # act - load and pivot
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  sql_table = copy_r_to_sql(db_conn, table_db, our_schema, table_name, in_data_table, query_path = query_path)
  
  pivoted_table = pivot_table(sql_table, label_column = "labels", value_column = "values", aggregator = "SUM")
  pivoted_table = dplyr::collect(pivoted_table)
  
  spread_table = tidyr::spread(in_data_table, labels, values)
  
  delete_table(db_conn, table_db, our_schema, table_name, query_path = query_path)
  close_database_connection(db_conn)
  
  # assert
  testthat::expect_true(all_equal(pivoted_table, out_data_table, ignore_row_order = TRUE, ignore_col_order = TRUE))
  testthat::expect_true(all_equal(pivoted_table, spread_table, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

## collapse indicators ---------------------------------------------------- ----

testthat::test_that("collapse indicator columns runs for remote data frames", {
  # arrange
  input_tbl = data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 1, 1)
  )
  prefix = "v_"
  label = "v"
  yes_value = 1
  
  expected_output_tbl = data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  
  # act
  table_name = "[4217154]"
  
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  sql_table = copy_r_to_sql(db_conn, table_db, our_schema, table_name, input_tbl, query_path = query_path)
  
  actual_output_tbl = collapse_indicator_columns(sql_table, prefix, yes_value, label)
  actual_output_tbl = dplyr::collect(actual_output_tbl)
  
  delete_table(db_conn, table_db, our_schema, table_name, query_path = query_path)
  close_database_connection(db_conn)
  
  # assert
  testthat::expect_true(all_equal(actual_output_tbl, expected_output_tbl, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

testthat::test_that("collapse indicator columns runs for remote data frames with multiple yes values", {
  # arrange
  input_tbl = data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 2, 2)
  )
  prefix = "v_"
  label = "v"
  yes_value = c(1, 2)
  
  expected_output_tbl = data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  
  # act
  table_name = "[test4482518]"
  
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  sql_table = copy_r_to_sql(db_conn, table_db, our_schema, table_name, input_tbl, query_path = query_path)
  
  actual_output_tbl = collapse_indicator_columns(sql_table, prefix, yes_value, label)
  actual_output_tbl = dplyr::collect(actual_output_tbl)
  
  delete_table(db_conn, table_db, our_schema, table_name, query_path = query_path)
  close_database_connection(db_conn)
  
  # assert
  testthat::expect_true(all_equal(actual_output_tbl, expected_output_tbl, ignore_row_order = TRUE, ignore_col_order = TRUE))
})
