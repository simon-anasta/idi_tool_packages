################################################################################
# Description: Automated tests for assembly tool validation functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
# - expect_error and expect_warning only match at most one error or warning.
#   Hence, we use suppressWarnings(expect_warnings(...)) when multiple warnings
#   can occur. See: `?expect_error` for documentation.
#
# Issues:
#
################################################################################

## setup in common ------------------------------------------------------ ----

path = system.file("extdata", "testing_control_validation", package = "assembly.tool")

# load control files
control_population = read.csv(file.path(path, "control_population_and_period.csv"), stringsAsFactors = FALSE)
control_measure = read.csv(file.path(path, "control_measures.csv"), stringsAsFactors = FALSE)
# as character
control_population[] = lapply(control_population, as.character)
control_measure[] = lapply(control_measure, as.character)
# lower case column names
colnames(control_population) = tolower(colnames(control_population))
colnames(control_measure) = tolower(colnames(control_measure))

# database connection
path = system.file("extdata", "testing_sqlite", package = "assembly.tool")
db_path = file.path(path, "testing_sqlite.db")
db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
on.exit(DBI::dbDisconnect(db_conn))

## validate_population_control_table(population_table) -------------------- ----

test_that("correct population passes", {
  expect_silent(validate_population_control_table(control_population))
  expect_false(validate_population_control_table(control_population))
})

test_that("swapped control files fails", {
  suppressWarnings(expect_warning(validate_population_control_table(control_measure)))
  expect_true(suppressWarnings(validate_population_control_table(control_measure)))
})

test_that("wrong columns warns", {
  tmp = dplyr::rename(control_population, wrong = label_identity)
  suppressWarnings(expect_warning(validate_population_control_table(tmp)))
  expect_true(suppressWarnings(validate_population_control_table(tmp)))
})

test_that("missing value warns", {
  tmp = control_population; tmp[1,1] = NA
  suppressWarnings(expect_warning(validate_population_control_table(tmp)))
  expect_true(suppressWarnings(validate_population_control_table(tmp)))
})

test_that("missing delim warns", {
  tmp = control_population; tmp[1,1] = "IDI_Sandpit"
  suppressWarnings(expect_warning(validate_population_control_table(tmp)))
  expect_true(suppressWarnings(validate_population_control_table(tmp)))
})

test_that("incorrect delim warns", {
  tmp = control_population; tmp[1,3] = "\"tmp_project_population\""
  suppressWarnings(expect_warning(validate_population_control_table(tmp)))
  expect_true(suppressWarnings(validate_population_control_table(tmp)))
})

test_that("internal delim warns", {
  tmp = control_population; tmp[1,5] = "\"be'fore\""
  suppressWarnings(expect_warning(validate_population_control_table(tmp)))
  expect_true(suppressWarnings(validate_population_control_table(tmp)))
})

## validate_measure_control_table_1(measure_table) ------------------------ ----

test_that("correct measure passes", {
  expect_silent(validate_measure_control_table_1(control_measure))
  expect_false(validate_measure_control_table_1(control_measure))
})

test_that("swapped control files fails", {
  suppressWarnings(expect_warning(validate_measure_control_table_1(control_population)))
  expect_true(suppressWarnings(validate_measure_control_table_1(control_population)))
})

test_that("wrong columns warns", {
  tmp = dplyr::rename(control_measure, wrong = measure_period_end_date)
  suppressWarnings(expect_warning(validate_measure_control_table_1(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_1(tmp)))
})

test_that("missing value warns", {
  tmp = control_measure; tmp[3,2] = NA
  suppressWarnings(expect_warning(validate_measure_control_table_1(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_1(tmp)))
})

test_that("missing delim warns", {
  tmp = control_measure; tmp[4,8] = "1"
  suppressWarnings(expect_warning(validate_measure_control_table_1(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_1(tmp)))
})

test_that("incorrect delim warns", {
  tmp = control_measure; tmp[3,3] = "\"tmp_accidents\""
  suppressWarnings(expect_warning(validate_measure_control_table_1(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_1(tmp)))
})

test_that("internal delim warns", {
  tmp = control_measure; tmp[4,3] = "[tmp[_]accidents]"
  suppressWarnings(expect_warning(validate_measure_control_table_1(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_1(tmp)))
})

## validate_control_table(control_table, type, column_reqs) --------------- ----
#
# Limited testing required as this function is wrapped by
# validate_population_control_table and validate_measure_control_table_1
# hence their tests also test this function.

test_that("accepted types enforced", {
  column_reqs = data.frame(a = 1:2, b = c("q","w"))
  expect_error(validate_control_table(control_population, "measur", column_reqs), "measure")
  expect_error(validate_control_table(control_population, "popul", column_reqs), "population")
})

## validate_measure_control_table_2(measure_table) ------------------------ ----

test_that("correct measure passes", {
  expect_silent(validate_measure_control_table_2(control_measure))
  expect_false(validate_measure_control_table_2(control_measure))
})

test_that("swapped control files fails", {
  suppressWarnings(expect_warning(validate_measure_control_table_2(control_population)))
  expect_true(suppressWarnings(validate_measure_control_table_2(control_population)))
})

test_that("wrong columns warns", {
  tmp = dplyr::rename(control_measure, wrong = proportional)
  suppressWarnings(expect_warning(validate_measure_control_table_2(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_2(tmp)))
})

test_that("missing value warns", {
  tmp = control_measure; tmp$measure_summarised_by[3] = NA
  suppressWarnings(expect_warning(validate_measure_control_table_2(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_2(tmp)))
})

test_that("incorrect terms warn", {
  tmp = control_measure; tmp$measure_summarised_by[3] = "wrong"
  suppressWarnings(expect_warning(validate_measure_control_table_2(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_2(tmp)))
  
  tmp = control_measure; tmp$proportional[3] = "wrong"
  suppressWarnings(expect_warning(validate_measure_control_table_2(tmp)))
  expect_true(suppressWarnings(validate_measure_control_table_2(tmp)))
})

## validate_database_tables(control_table, db_connection) --------------- ----

test_that("correct control files pass", {
  tmp = control_population
  tmp$database_name = "[]"
  tmp$schema_name = "[]"
  expect_silent(validate_database_tables(tmp, db_conn))
  expect_false(validate_database_tables(tmp, db_conn))
  
  tmp = control_measure
  tmp$database_name = "[]"
  tmp$schema_name = "[]"
  expect_silent(validate_database_tables(tmp, db_conn))
  expect_false(validate_database_tables(tmp, db_conn))
})

test_that("wrong table warns", {
  tmp = control_population
  tmp$database_name = "[]"
  tmp$schema_name = "[]"
  tmp$table_name[2] = "[wrong]"
  
  expect_warning(validate_database_tables(tmp, db_conn))
  expect_true(suppressWarnings(validate_database_tables(tmp, db_conn)))
})

test_that("wrong column warns", {
  tmp = control_measure
  tmp$database_name = "[]"
  tmp$schema_name = "[]"
  tmp$measure_period_start_date[8] = "[wrong]"
  
  expect_warning(validate_database_tables(tmp, db_conn))
  expect_true(suppressWarnings(validate_database_tables(tmp, db_conn)))
})

test_that("non-numeric column warns", {
  tmp = control_measure
  tmp$database_name = "[]"
  tmp$schema_name = "[]"
  tmp$value_measure[13] = "[benefit_end_date]"
  
  expect_warning(validate_database_tables(tmp, db_conn))
  expect_true(suppressWarnings(validate_database_tables(tmp, db_conn)))
})
