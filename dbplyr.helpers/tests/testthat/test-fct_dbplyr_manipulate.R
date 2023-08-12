################################################################################
# Description: Automated tests for dbplyr manipulate database tables
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## delete_table(db_connection, db, schema, tbl_name, mode = "table") ------ ----

test_that("tables deleted", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS delete_table_test")
  DBI::dbExecute(db_conn, "CREATE TABLE delete_table_test AS SELECT * FROM iris_table")
  table_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[delete_table_test]")
  
  delete_table(db_conn, tbl_name = "[delete_table_test]", mode = "table")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[delete_table_test]")
  
  # assert
  expect_true(table_exists)
  expect_false(table_remains)
  
  DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS delete_table_test")
})

test_that("views deleted", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  DBI::dbExecute(db_conn, "DROP VIEW IF EXISTS delete_view_test")
  DBI::dbExecute(db_conn, "CREATE VIEW delete_view_test AS SELECT * FROM iris_table")
  table_exists = table_or_view_exists_in_db(db_conn, tbl_name = "[delete_view_test]")
  
  delete_table(db_conn, tbl_name = "[delete_view_test]", mode = "view")
  table_remains = table_or_view_exists_in_db(db_conn, tbl_name = "[delete_view_test]")
  
  # assert
  expect_true(table_exists)
  expect_false(table_remains)
  
  DBI::dbExecute(db_conn, "DROP VIEW IF EXISTS delete_view_test")
})

## create_nonclustered_index(db_connection, db, schema, tbl_name, index_columns) ----
#
# Skip as no good way to test.

## compress_table(db_connection, db, schema, tbl_name) -------------------- ----
#
# Skip as no good way to test.

## union_all(table_a, table_b, list_of_columns) --------------------------- ----

test_that("union all combines", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  iris_table = create_access_point(db_conn, tbl_name = "[iris_table]")
  iris_head = utils::head(iris_table, 10)
  
  # act
  combined_table = union_all(iris_head, iris_head, colnames(iris_table))
  size = dplyr::summarise(combined_table, num = dplyr::n())
  size = dplyr::collect(size)
  
  # assert
  expect_equal(size[[1,1]], 20)
})

## pivot_table(input_tbl, label_column, value_column, aggregator = "SUM") - ----

# # setup
# path = system.file("extdata", "testing", package = "dbplyr.helpers")
# db_path = file.path(path, "testing_sqlite.db")
# db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
# 
# table_long_thin = data.frame(
#   people = c("bob", "alice", "bob", "alice"),
#   labels = c("age", "age", "height", "height"),
#   values = c(10, 12, 150, 160),
#   stringsAsFactors = FALSE
# )
# 
# DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS table_long_thin")
# DBI::dbWriteTable(db_conn, "table_long_thin", table_long_thin)
# 
# DBI::dbDisconnect(db_conn)

test_that("table pivoted", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  remote_long_thin = create_access_point(db_conn, tbl_name = "[table_long_thin]")
  
  actual_rectangular = pivot_table(remote_long_thin, label_column = "labels", value_column = "values", aggregator = "sum")
  actual_rectangular = dplyr::collect(actual_rectangular)
  
  expected_rectangular = data.frame(
    people = c("bob", "alice"),
    age = c(10, 12),
    height = c(150, 160),
    stringsAsFactors = FALSE
  )
  expected_rectangular = tibble::as_tibble(expected_rectangular)
  
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_rectangular, people),
    dplyr::arrange(expected_rectangular, people)
  ))
})

## collapse_indicator_columns(input_tbl, prefix, yes_values, label = prefix) ----

# # setup
# path = system.file("extdata", "testing", package = "dbplyr.helpers")
# db_path = file.path(path, "testing_sqlite.db")
# db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
# 
# table_w_indicators = data.frame(
#   id = c(1, 2, 3),
#   v_a = c(1, 0, 0),
#   v_b = c(0, 2, 0),
#   v_c = c(0, 1, 1)
# )
# 
# DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS table_w_indicators")
# DBI::dbWriteTable(db_conn, "table_w_indicators", table_w_indicators)
# 
# DBI::dbDisconnect(db_conn)

test_that("indicators collapsed for local data frames", {
  table_w_indicators = data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 2, 0),
    v_c = c(0, 1, 1)
  )
  
  actual_output_tbl = collapse_indicator_columns(table_w_indicators, prefix = "v_", yes_values = 1, label = "w")
  
  expected_output_tbl = data.frame(id = c(1, 2, 3), w = c("a", "c", "c"), stringsAsFactors = FALSE)
  
  expect_true(all.equal(
    actual_output_tbl[rownames(expected_output_tbl), names(expected_output_tbl)],
    expected_output_tbl
  ))
})

test_that("indicators collapsed for local data frames with multiple yes values", {
  table_w_indicators = data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 2, 0),
    v_c = c(0, 1, 1)
  )
  
  actual_output_tbl = collapse_indicator_columns(table_w_indicators, prefix = "v_", yes_values = c(1,2), label = "w")
  
  expected_output_tbl = data.frame(id = c(1, 2, 3), w = c("a", "b", "c"), stringsAsFactors = FALSE)
  
  expect_true(all.equal(
    actual_output_tbl[rownames(expected_output_tbl), names(expected_output_tbl)],
    expected_output_tbl
  ))
})

test_that("indicators collapsed for remote data frames", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  remote_table_w_indicators = create_access_point(db_conn, tbl_name = "[table_w_indicators]")
  
  # act
  actual_output_tbl = collapse_indicator_columns(remote_table_w_indicators, prefix = "v_", yes_values = 1, label = "w")
  actual_output_tbl = dplyr::collect(actual_output_tbl)
  
  expected_output_tbl = data.frame(id = c(1, 2, 3), w = c("a", "c", "c"), stringsAsFactors = FALSE)
  expected_output_tbl = tibble::as_tibble(expected_output_tbl)

  # assert
  expect_true(all(class(actual_output_tbl) == class(expected_output_tbl)))
  
  expect_true(all.equal(
    actual_output_tbl[rownames(expected_output_tbl), names(expected_output_tbl)],
    expected_output_tbl
  ))
})

test_that("indicators collapsed for remote data frames with multiple yes values", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  remote_table_w_indicators = create_access_point(db_conn, tbl_name = "[table_w_indicators]")
  
  # act
  actual_output_tbl = collapse_indicator_columns(remote_table_w_indicators, prefix = "v_", yes_values = c(1, 2), label = "w")
  actual_output_tbl = dplyr::collect(actual_output_tbl)
  
  expected_output_tbl = data.frame(id = c(1, 2, 3), w = c("a", "b", "c"), stringsAsFactors = FALSE)
  expected_output_tbl = tibble::as_tibble(expected_output_tbl)
  
  # assert
  expect_true(all(class(actual_output_tbl) == class(expected_output_tbl)))
  
  expect_true(all.equal(
    actual_output_tbl[rownames(expected_output_tbl), names(expected_output_tbl)],
    expected_output_tbl
  ))
})

## tidy afterwards -------------------------------------------------------- ----

# remove tmp scripts folder
tmp_directory <- "./SQL tmp scripts"
unlink(tmp_directory, recursive = TRUE)
