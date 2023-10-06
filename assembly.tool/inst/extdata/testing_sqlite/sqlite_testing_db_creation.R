################################################################################
# Description: Create SQLite database for testing
# Author: Simon Anastasiadis
#
# Notes:
# - Run to setup basic database for package testing
#
################################################################################

## setup ------------------------------------------------------------------ ----

required_packages = c("DBI", "RSQLite", "dbplyr.helpers")
stopifnot(all(required_packages %in% installed.packages()))

path = system.file("extdata", "testing_sqlite", package = "assembly.tool")
db_path = file.path(path, "testing_sqlite.db")
db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)

## load data csv's to db -------------------------------------------------- ----

data_accidents = read.csv(file.path(path, "data_accidents.csv"), stringsAsFactors = FALSE)
data_benefit_payment = read.csv(file.path(path, "data_benefit_payment.csv"), stringsAsFactors = FALSE)
data_project_population = read.csv(file.path(path, "data_project_population.csv"), stringsAsFactors = FALSE)

dbplyr.helpers::copy_r_to_sql(db_conn, sql_table_name = "[tmp_accidents]", r_table_name = data_accidents)
dbplyr.helpers::copy_r_to_sql(db_conn, sql_table_name = "[tmp_benefit_payment]", r_table_name = data_benefit_payment)
dbplyr.helpers::copy_r_to_sql(db_conn, sql_table_name = "[tmp_project_population]", r_table_name = data_project_population)

## conclude --------------------------------------------------------------- ----

# disconnect
DBI::dbDisconnect(db_conn)
