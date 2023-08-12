################################################################################
# Description: Automated tests for dbplyr write to database functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## create_table(db_connection, db, schema, tbl_name, named_list_of_columns, OVERWRITE = FALSE) ----

test_that("tables are created", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  already_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[create_table_test]")
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS create_table_test")
  
  named_list_of_columns <- list(
    number = "[int] NOT NULL",
    date = "[date] NOT NULL",
    character = "[varchar](25) NULL"
  )
  
  create_table(db_conn, tbl_name = "[create_table_test]", named_list_of_columns = named_list_of_columns)
  now_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[create_table_test]")
  
  # assert
  expect_false(already_exists)
  expect_true(now_exists)
  
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS create_table_test")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[create_table_test]")
  expect_false(table_remains)
})

## append_database_table(table_to_append, db_connection, db, schema, tbl_name, list_of_columns) ----

test_that("results are appended", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  already_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[append_table_test]")
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS append_table_test")
  
  named_list_of_columns <- list(
    Sepal.Length = "[float] NOT NULL",
    Sepal.Width = "[float] NOT NULL",
    Petal.Length = "[float] NOT NULL",
    Petal.Width = "[float] NOT NULL",
    Species = "[varchar](15) NULL"
  )
  
  create_table(db_conn, tbl_name = "[append_table_test]", named_list_of_columns = named_list_of_columns)
  now_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[append_table_test]")
  initial_size = DBI::dbGetQuery(db_conn, "SELECT COUNT(*) AS num FROM append_table_test")
  
  to_append = create_access_point(db_conn, tbl_name = "[iris_table]")
  append_database_table(to_append, db_conn, tbl_name = "[append_table_test]", list_of_columns = colnames(to_append))
  new_size = DBI::dbGetQuery(db_conn, "SELECT COUNT(*) AS num FROM append_table_test")
  
  # assert
  expect_false(already_exists)
  expect_true(now_exists)
  
  expect_equal(initial_size[[1,1]], 0)
  expect_equal(new_size[[1,1]], 150)
  
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS append_table_test")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[append_table_test]")
  expect_false(table_remains)
})

## write_to_database(input_tbl, db_connection, db, schema, tbl_name, OVERWRITE = FALSE) ----

test_that("new tables written", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  iris_table = create_access_point(db_conn, tbl_name = "[iris_table]")
  iris_head = utils::head(iris_table)
  
  # act
  already_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[write_table_test]")
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS write_table_test")
  
  test_table = write_to_database(iris_head, db_conn, tbl_name = "[write_table_test]", OVERWRITE = FALSE)
  now_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[write_table_test]")
  
  # assert
  expect_false(already_exists)
  expect_true(now_exists)
  
  expect_error(write_to_database(iris_head, db_conn, tbl_name = "[write_table_test]", OVERWRITE = FALSE))
  expect_true("tbl_sql" %in% class(test_table))
  
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS write_table_test")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[write_table_test]")
  expect_false(table_remains)
})

## write_for_reuse(tbl_to_save, db_connection, db, schema, tbl_name, index_columns = NA, print_off = TRUE) ----
#
# not tested separately as just runs several other functions in sequence.

## copy_r_to_sql(db_connection, db, schema, sql_table_name, r_table_name, OVERWRITE = FALSE) ----

test_that("data copied from R", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  already_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[copy_r_to_sql_test]")
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS copy_r_to_sql_test")
  
  local_df = data.frame(
    a = 1:5,
    b = c("a","b","c","d","e"),
    stringsAsFactors = FALSE
  )
  remote_tbl = copy_r_to_sql(db_conn, sql_table_name = "[copy_r_to_sql_test]", r_table_name = local_df)
  
  now_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[copy_r_to_sql_test]")
  
  # assert
  expect_false(already_exists)
  expect_true(now_exists)
  expect_true(dplyr::is.tbl(remote_tbl))
  expect_true("tbl_sql" %in% class(remote_tbl))
  
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS copy_r_to_sql_test")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[copy_r_to_sql_test]")
  expect_false(table_remains)
})

## create_view(tbl_name, db_connection, db, schema, view_name, OVERWRITE = FALSE) ----

test_that("views can be created", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  already_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[create_view_test]")
  DBI::dbExecute(db_conn, "DROP VIEW IF EXISTS create_view_test")
  
  remote_df = create_access_point(db_conn, tbl_name = "[iris_table]")
  remote_df = dplyr::filter(remote_df, Species == "virginica")
  
  test_view = create_view(remote_df, db_conn, view_name = "[create_view_test]")
  
  now_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[create_view_test]")
  
  # assert
  expect_false(already_exists)
  expect_true(now_exists)
  expect_true(dplyr::is.tbl(test_view))
  expect_true("tbl_sql" %in% class(test_view))
  
  DBI::dbExecute(db_conn, "DROP VIEW IF EXISTS create_view_test")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[create_view_test]")
  expect_false(table_remains)
})

## tidy afterwards -------------------------------------------------------- ----

# remove tmp scripts folder
tmp_directory <- "./SQL tmp scripts"
unlink(tmp_directory, recursive = TRUE)
