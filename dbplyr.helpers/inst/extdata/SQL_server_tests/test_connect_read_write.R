################################################################################
# Description: SQL Server tests - connect, read, write
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Testing the following functions that handle basic SQL read/write
#    - create_access_point(db_connection, schema, tbl_name)
#    - copy_r_to_sql(db_connection, schema, sql_table_name, r_table_name, OVERWRITE = FALSE)
#    - delete_table(db_connection, schema, tbl_name, mode = "table")
#    - close_database_connection(db_connection)
#    - table_or_view_exists_in_db(db_connection, db, schema, tbl_name)
#
# Issues:
#
################################################################################

testthat::test_that("connection can see tables in database", {
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  tables_in_db = DBI::dbListTables(db_conn)
  testthat::expect_true(length(tables_in_db) > 0)
  
  close_database_connection(db_conn)
  testthat::expect_error(DBI::dbListTables(db_conn))
})

testthat::test_that("table non-existence can be determined", {
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  testthat::expect_false(table_or_view_exists_in_db(db_conn, "[made_up_name]", "[made_up_name]", "[made_up_name]"))
  close_database_connection(db_conn)
})

testthat::test_that("dbplyr writes, reads, and deletes", {
  # Arrange
  table_name = "[test2777647]"
  data(cars)
  
  # Act
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  remote_table = copy_r_to_sql(db_conn, table_db, our_schema, table_name, cars, query_path = query_path)
  table_written_to_sql = table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  
  local_table = dplyr::collect(create_access_point(db_conn, table_db, our_schema, table_name))
  origin_and_read_table_identical = all_equal(cars, local_table, ignore_row_order = TRUE)
  
  delete_table(db_conn, table_db, our_schema, table_name, mode = "table", query_path = query_path)
  table_deleted_from_sql = !table_or_view_exists_in_db(db_conn, table_db, our_schema, table_name)
  close_database_connection(db_conn)
  
  # Assert
  testthat::expect_true(table_written_to_sql)
  testthat::expect_true(origin_and_read_table_identical)
  testthat::expect_true(table_deleted_from_sql)
})