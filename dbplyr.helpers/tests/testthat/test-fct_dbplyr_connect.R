################################################################################
# Description: Automated tests for dbplyr database connection functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## display_connection_guidance() ------------------------------------------ ----

test_that("guidance displayed", {
  expect_output(display_connection_guidance())
})

## create_access_point(db_connection, db = "", schema = "", tbl_name) ----- ----

test_that("connections created", {
  
  # arrange
  path = system.file("extdata", "testing", package = "dbplyr.helpers")
  db_path = file.path(path, "testing_sqlite.db")
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn))
  
  # act
  remote_tbl = create_access_point(db_conn, tbl_name = "[iris_table]")
  
  # assert
  expect_true(dplyr::is.tbl(remote_tbl))
  expect_true("tbl_sql" %in% class(remote_tbl))
  
  data(iris)
  expect_true(all(colnames(iris) == colnames(remote_tbl)))
})
