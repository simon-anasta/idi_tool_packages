################################################################################
# Description: Create SQLite database for testing
# Author: Simon Anastasiadis
#
# Notes:
# - Run to setup basic database for package testing
#
################################################################################

## setup ------------------------------------------------------------------ ----

required_packages = c("DBI", "RSQLite")
stopifnot(all(required_packages %in% installed.packages()))

path = system.file("extdata", "testing", package = "dbplyr.helpers")
db_path = file.path(path, "testing_sqlite.db")
db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)

## general use table & view ----------------------------------------------- ----

# add table
data(iris)
DBI::dbWriteTable(db_conn, "iris_table", iris)

# add view
DBI::dbExecute(db_conn, "CREATE VIEW iris_view AS SELECT * FROM iris_data WHERE Species = 'setosa'")

# confirm contents
tbls = DBI::dbListTables(db_conn)
print(tbls)

## for pivot test --------------------------------------------------------- ----

table_long_thin = data.frame(
  people = c("bob", "alice", "bob", "alice"),
  labels = c("age", "age", "height", "height"),
  values = c(10, 12, 150, 160),
  stringsAsFactors = FALSE
)

DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS table_long_thin")
DBI::dbWriteTable(db_conn, "table_long_thin", table_long_thin)

## for collapse indicator columns test ------------------------------------ ----

table_w_indicators = data.frame(
  id = c(1, 2, 3),
  v_a = c(1, 0, 0),
  v_b = c(0, 2, 0),
  v_c = c(0, 1, 1)
)

DBI::dbExecute(db_conn, "DROP TABLE IF EXISTS table_w_indicators")
DBI::dbWriteTable(db_conn, "table_w_indicators", table_w_indicators)

## conclude --------------------------------------------------------------- ----

# disconnect
DBI::dbDisconnect(db_conn)
