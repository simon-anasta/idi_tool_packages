################################################################################
# Description: SQL Server tests - other writing
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Testing the following functions that handle basic SQL read/write
#    - write_to_database(input_tbl, db_connection, schema, tbl_name, OVERWRITE = FALSE)
#    - create_table(db_connection, schema, tbl_name, named_list_of_columns, OVERWRITE = FALSE)
#    - create_view(tbl_name, db_connection, schema, view_name, OVERWRITE = FALSE)
#    - append_database_table(db_connection, schema, tbl_name, list_of_columns, table_to_append)
#
# Issues:
#
################################################################################

## creation and appending ------------------------------------------------- ----

testthat::test_that("sql accepts creation and appending", {
  # arrange
  table_name1 = "[test_tbl2394347]"
  table_name2 = "[test_tbl9643068]"
  
  named_list_of_columns = list(number = "[int] NOT NULL", date = "[date] NOT NULL", character = "[varchar](25) NULL")
  
  table_data = data.frame(
    number = c(1, 2, 3),
    date = c("2000-02-29", "2001-01-01", "2003-12-31"),
    character = c("a", "b", "c"), stringsAsFactors = FALSE
  )
  
  # act - blank table
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  
  dbplyr.helpers::create_table(db_conn, table_db, our_schema, table_name1, named_list_of_columns, OVERWRITE = TRUE, query_path = query_path)
  table_created_in_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name1)
  
  new_table = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, table_name1)
  new_table_row_count = dplyr::ungroup(new_table)
  new_table_row_count = dplyr::summarise(new_table_row_count, num = dplyr::n())
  new_table_row_count = unlist(dplyr::collect(new_table_row_count), use.names = FALSE)
  
  # act - append
  remote_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, table_name2, table_data, OVERWRITE = TRUE, query_path = query_path)
  remote_table_row_count = dplyr::ungroup(remote_table)
  remote_table_row_count = dplyr::summarise(remote_table_row_count, num = n())
  remote_table_row_count = unlist(dplyr::collect(remote_table_row_count), use.names = FALSE)
  
  dbplyr.helpers::append_database_table(remote_table, db_conn, table_db, our_schema, table_name1, colnames(table_data), query_path = query_path)
  appended_table_row_count = dplyr::ungroup(new_table)
  appended_table_row_count = dplyr::summarise(appended_table_row_count, num = n())
  appended_table_row_count = unlist(dplyr::collect(appended_table_row_count), use.names = FALSE)
  
  # act - delete & tidy up
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, table_name1, mode = "table", query_path = query_path)
  table1_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name1)
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, table_name2, mode = "table", query_path = query_path)
  table2_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name2)
  DBI::dbDisconnect(db_conn)
  
  # assert
  testthat::expect_true(table_created_in_sql)
  testthat::expect_equal(new_table_row_count, 0)
  testthat::expect_equal(appended_table_row_count, nrow(table_data))
  testthat::expect_equal(remote_table_row_count, nrow(table_data))
  testthat::expect_true(table1_deleted_from_sql)
  testthat::expect_true(table2_deleted_from_sql)
})

## new tables ------------------------------------------------------------- ----

testthat::test_that("new tables can be written", {
  copied_table_name = "[test_tbl2515936]"
  written_table_name = "[test_tbl1196794]"
  data(cars)
  
  # act - copy in
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  initial_copied_table = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, copied_table_name)
  copied_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, copied_table_name, cars, OVERWRITE = initial_copied_table, query_path = query_path)
  table_copied_to_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, copied_table_name)
  
  # act - rewrite
  initial_written_table = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, written_table_name)
  written_table = dbplyr.helpers::write_to_database(copied_table, db_conn, table_db, our_schema, written_table_name, OVERWRITE = initial_written_table, query_path = query_path)
  table_written_to_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, written_table_name)
  
  # act - delete & tidy up
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, copied_table_name, mode = "table", query_path = query_path)
  copied_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, copied_table_name)
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, written_table_name, mode = "table", query_path = query_path)
  written_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, written_table_name)
  DBI::dbDisconnect(db_conn)
  
  # assert
  testthat::expect_true(table_copied_to_sql)
  testthat::expect_true(table_written_to_sql)
  testthat::expect_true(copied_deleted_from_sql)
  testthat::expect_true(written_deleted_from_sql)
})

## views ------------------------------------------------------------------ ----

testthat::test_that("views can be created and deleted", {
  table_name = "[test_tbl4975569]"
  view_name = "[test_view4294783]"
  data(cars)
  
  # act - view
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  
  initial_table = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  test_table = dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, table_name, cars, OVERWRITE = initial_table, query_path = query_path)
  table_copied_to_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  
  initial_view = dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, view_name)
  test_view = dbplyr.helpers::create_view(test_table, db_conn, view_db, our_schema, view_name, OVERWRITE = initial_view, query_path = query_path)
  view_in_sql = dbplyr.helpers::table_or_view_exists_in_db(db_conn, view_db, our_schema, view_name)
  
  # act - removal
  dbplyr.helpers::delete_table(db_conn, table_db, our_schema, table_name, mode = "table", query_path = query_path)
  table_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  dbplyr.helpers::delete_table(db_conn, view_db, our_schema, view_name, mode = "view", query_path = query_path)
  view_deleted_from_sql = !dbplyr.helpers::table_or_view_exists_in_db(db_conn, view_db, our_schema, view_name)
  DBI::dbDisconnect(db_conn)
  
  # assert
  testthat::expect_true(view_in_sql)
  testthat::expect_true(table_deleted_from_sql)
  testthat::expect_true(view_deleted_from_sql)
})
