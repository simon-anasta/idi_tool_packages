################################################################################
# Description: Create SQLite database for testing
# Author: Simon Anastasiadis
#
# Notes:
# - Run to setup basic database for package testing
#
################################################################################

required_packages = c("DBI", "RSQLite")
stopifnot(all(required_packages %in% installed.packages()))

path = system.file("extdata", "testing", package = "dbplyr.helpers")
db_path = file.path(path, "testing_sqlite.db")
db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)

# add table
data(iris)
DBI::dbWriteTable(db_conn, "iris_table", iris)

# add view
DBI::dbExecute(db_conn, "CREATE VIEW iris_view AS SELECT * FROM iris_data WHERE Species = 'setosa'")

# confirm contents
tbls = DBI::dbListTables(db_conn)
print(tbls)

# disconnect
DBI::dbDisconnect(db_conn)
