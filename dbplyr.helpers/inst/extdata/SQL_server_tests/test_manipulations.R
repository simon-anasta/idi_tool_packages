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
  initial_table1 = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, head_table_name)
  initial_table2 = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, tail_table_name)
  
  head_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, head_table_name, head_cars, OVERWRITE = initial_table1, query_path = query_path)
  tail_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, tail_table_name, tail_cars, OVERWRITE = initial_table2, query_path = query_path)
  
  table_written_to_sql1 = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, head_table_name)
  table_written_to_sql2 = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, tail_table_name)
  
  unioned_table = dbplyr.helpers::union_all(head_table, tail_table, colnames(cars))
  unioned_table = dplyr::collect(unioned_table)
  
  rowbound_table = rbind(head_cars, tail_cars)
  
  # act - delete & tidy up
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, head_table_name, mode = "table", query_path = query_path)
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, tail_table_name, mode = "table", query_path = query_path)
  table_deleted_from_sql1 = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, head_table_name)
  table_deleted_from_sql2 = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, tail_table_name)
  DBI::dbDisconnect(db_conn)
  
  # assert
  unioned_table = as.data.frame(unioned_table)
  unioned_table = unioned_table[order(unioned_table$speed, unioned_table$dist),sort(colnames(unioned_table))]
  rowbound_table = rowbound_table[order(rowbound_table$speed, rowbound_table$dist),sort(colnames(rowbound_table))]
  row.names(unioned_table) = NULL
  row.names(rowbound_table) = NULL
  
  testthat::expect_true(all.equal(unioned_table, rowbound_table))
  
  testthat::expect_true(table_written_to_sql1)
  testthat::expect_true(table_written_to_sql2)
  testthat::expect_true(table_deleted_from_sql1)
  testthat::expect_true(table_deleted_from_sql2)
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
  initial_table = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  sql_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, table_name, in_data_table, OVERWRITE = initial_table, query_path = query_path)
  table_written_to_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  
  pivoted_table = dbplyr.helpers::pivot_table(sql_table, label_column = "labels", value_column = "values", aggregator = "SUM")
  pivoted_table = dplyr::collect(pivoted_table)
  
  spread_table = tidyr::spread(in_data_table, labels, values)
  
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, table_name, query_path = query_path)
  table_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  DBI::dbDisconnect(db_conn)
  
  # assert
  pivoted_table = as.data.frame(pivoted_table)
  spread_table = as.data.frame(spread_table)
  pivoted_table = pivoted_table[order(pivoted_table$people),sort(colnames(pivoted_table))]
  out_data_table = out_data_table[order(out_data_table$people),sort(colnames(out_data_table))]
  spread_table = spread_table[order(spread_table$people),sort(colnames(spread_table))]
  row.names(pivoted_table) = NULL
  row.names(out_data_table) = NULL
  row.names(spread_table) = NULL
  
  testthat::expect_true(all.equal(pivoted_table, out_data_table))
  testthat::expect_true(all.equal(pivoted_table, spread_table))
  testthat::expect_true(all.equal(out_data_table, spread_table))
  
  testthat::expect_true(table_written_to_sql)
  testthat::expect_true(table_deleted_from_sql)
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
  table_name = "[test4217154]"
  
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  initial_table = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  sql_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, table_name, input_tbl, OVERWRITE = initial_table, query_path = query_path)
  table_written_to_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  
  actual_output_tbl = dbplyr.helpers::collapse_indicator_columns(sql_table, prefix, yes_value, label)
  actual_output_tbl = dplyr::collect(actual_output_tbl)
  
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, table_name, query_path = query_path)
  table_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  DBI::dbDisconnect(db_conn)
  
  # assert
  actual_output_tbl = as.data.frame(actual_output_tbl)
  actual_output_tbl = actual_output_tbl[order(actual_output_tbl$id),sort(colnames(actual_output_tbl))]
  expected_output_tbl = expected_output_tbl[order(expected_output_tbl$id),sort(colnames(expected_output_tbl))]
  row.names(actual_output_tbl) = NULL
  row.names(expected_output_tbl) = NULL
  
  testthat::expect_true(all.equal(actual_output_tbl, expected_output_tbl))
  
  testthat::expect_true(table_written_to_sql)
  testthat::expect_true(table_deleted_from_sql)
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
  initial_table = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  sql_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, table_name, input_tbl, OVERWRITE = initial_table, query_path = query_path)
  table_written_to_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  
  actual_output_tbl = dbplyr.helpers::collapse_indicator_columns(sql_table, prefix, yes_value, label)
  actual_output_tbl = dplyr::collect(actual_output_tbl)
  
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, table_name, query_path = query_path)
  table_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  DBI::dbDisconnect(db_conn)
  
  # assert
  actual_output_tbl = as.data.frame(actual_output_tbl)
  actual_output_tbl = actual_output_tbl[order(actual_output_tbl$id),sort(colnames(actual_output_tbl))]
  expected_output_tbl = expected_output_tbl[order(expected_output_tbl$id),sort(colnames(expected_output_tbl))]
  row.names(actual_output_tbl) = NULL
  row.names(expected_output_tbl) = NULL
  
  testthat::expect_true(all.equal(actual_output_tbl, expected_output_tbl))
  
  testthat::expect_true(table_written_to_sql)
  testthat::expect_true(table_deleted_from_sql)
})
