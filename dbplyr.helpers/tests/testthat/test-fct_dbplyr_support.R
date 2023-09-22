################################################################################
# Description: Automated tests for dbplyr supporting functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## db_schema(db, schema) -------------------------------------------------- ----

test_that("db and schema are delimited", {
  expect_silent(db_schema("[foo]", "[bar]"))
  expect_warning(db_schema("foo", "[bar]"), "db")
  expect_warning(db_schema("[foo]", "bar"), "schema")
})

test_that("db and schema are combined", {
  expect_equal(db_schema("[foo]", "[bar]"), "[foo].[bar]")
  expect_equal(db_schema("[foo]", ""), "[foo]")
  expect_equal(db_schema("", "[bar]"), "[bar]")
})

## warn_if_missing_delimiters(db, schema, tbl_name) ----------------------- ----

test_that("non-delimited sql objects produce warnings", {
  expect_silent(warn_if_missing_delimiters("[db]", "[schema]", "[tbl_name]"))
  expect_warning(warn_if_missing_delimiters("db", "[schema]", "[tbl_name]"), "db")
  expect_warning(warn_if_missing_delimiters("[db]", "schema", "[tbl_name]"), "schema")
  expect_warning(warn_if_missing_delimiters("[db]", "[schema]", "tbl_name"), "tbl_name")
})

test_that("empty objects do not warn", {
  expect_silent(warn_if_missing_delimiters("[]", "[schema]", "[tbl_name]"))
  expect_silent(warn_if_missing_delimiters("[]", "[]", "[tbl_name]"))
  expect_silent(warn_if_missing_delimiters("[]", "[]", "[]"))
  
})

test_that("blank objects do warn", {
  expect_warning(warn_if_missing_delimiters("", "[schema]", "[tbl_name]"), "db")
  expect_warning(warn_if_missing_delimiters("[db]", "", "[tbl_name]"), "schema")
  expect_warning(warn_if_missing_delimiters("[db]", "[schema]", ""), "tbl_name")
})

## table_or_view_exists_in_db(db_connection, db, schema, tbl_name) -------- ----

test_that("table and view detected", {
  
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  expect_true(table_or_view_exists_in_db(db_conn, tbl_name = "[iris_table]"))
  expect_true(table_or_view_exists_in_db(db_conn, tbl_name = "[iris_view]"))
  
  expect_false(table_or_view_exists_in_db(db_conn, tbl_name = "[nonexistant_table]"))
})

## table_contains_required_columns(tbl_to_check, required_columns, only = FALSE) ----

test_that("column names are checked", {
  data(iris)
  cols = colnames(iris)
  
  expect_true(table_contains_required_columns(iris, cols))
  expect_true(table_contains_required_columns(iris, cols[1]))
  expect_true(table_contains_required_columns(iris, cols, only = TRUE))
  
  expect_false(table_contains_required_columns(iris, cols[1], only = TRUE))
  expect_false(table_contains_required_columns(iris, c(cols, "asdasdasd"), only = TRUE))
  expect_false(table_contains_required_columns(iris, c(cols, "asdasdasd")))
})

## save_to_sql_script(query, desc, path = getwd()) ------------------------ ----

test_that("sql files written", {
  
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  
  # arrange
  tmp_directory = file.path(path, "SQL tmp scripts")
  folder_exists_to_start = dir.exists(tmp_directory)
  file_name = "test123456789"
  
  # act
  save_to_sql_script("placeholder query", file_name, query_path = tmp_directory)
  
  folder_exists = dir.exists(tmp_directory)
  file_exists = any(grepl(file_name, list.files(tmp_directory)))
  
  # tidy
  file_to_remove = dir(tmp_directory, pattern = file_name)
  file.remove(file.path(tmp_directory, file_to_remove))
  unlink(tmp_directory, recursive = TRUE)
  
  folder_exists_at_end = dir.exists(tmp_directory)
  
  # assert
  expect_true(file_exists)
  expect_true(folder_exists)
  expect_false(folder_exists_to_start)
  expect_false(folder_exists_at_end)
})

test_that("sql files not written", {
  
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  
  # arrange
  tmp_directory = file.path(path, "SQL tmp scripts")
  folder_exists_to_start = dir.exists(tmp_directory)
  file_name = "test123456789"
  
  # act
  save_to_sql_script("placeholder query", file_name, query_path = NA)
  
  folder_exists = dir.exists(tmp_directory)
  
  # tidy
  if(folder_exists){
    unlink(tmp_directory, recursive = TRUE)
  }
  
  folder_exists_at_end = dir.exists(tmp_directory)
  
  # assert
  expect_false(folder_exists_to_start)
  expect_false(folder_exists)
  expect_false(folder_exists_at_end)
})

