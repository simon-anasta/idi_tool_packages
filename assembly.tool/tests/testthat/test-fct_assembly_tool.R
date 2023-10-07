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

## setup in common -------------------------------------------------------- ----

# database connection
path = system.file("extdata", "testing_sqlite", package = "assembly.tool")
db_path = file.path(path, "testing_sqlite.db")
db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
on.exit(DBI::dbDisconnect(db_conn))

path = system.file("extdata", "testing_integration", package = "assembly.tool")
# load control files
control_population = read.csv(file.path(path, "control_population_and_period.csv"), stringsAsFactors = FALSE)
control_measure = read.csv(file.path(path, "control_measures.csv"), stringsAsFactors = FALSE)
# as character
control_population[] = lapply(control_population, as.character)
control_measure[] = lapply(control_measure, as.character)
# lower case column names
colnames(control_population) = tolower(colnames(control_population))
colnames(control_measure) = tolower(colnames(control_measure))
# remove database and schema names for SQLite
control_population$database_name = "[]"
control_population$schema_name = "[]"
control_measure$database_name = "[]"
control_measure$schema_name = "[]"


SILENT_PROGRESS = TRUE
SAVE_SCRIPTS = NA


## dataset_assembly_tool 1 ------------------------------------------------ ----

test_that("passes when running checks", {
  
  expect_silent(
    dataset_assembly_tool(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = "[]",
      output_schema = "[]",
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rectangular]",
      control_development_mode = FALSE,
      control_run_checks_only = TRUE,
      control_silence_progress = TRUE,
      control_append_long_thin = FALSE
    )
  )
  
  expect_output(
    dataset_assembly_tool(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = "[]",
      output_schema = "[]",
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rectangular]",
      control_development_mode = FALSE,
      control_run_checks_only = TRUE,
      control_silence_progress = FALSE,
      control_append_long_thin = FALSE
    )
  )  
  
})

## assemble_output_table -------------------------------------------------- ----

test_that("long-thin table produced", {
  
  assemble_output_table(
    population_control_table = control_population,
    measures_control_table = control_measure,
    db_connection = db_conn,
    output_database = "[]",
    output_schema = "[]",
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rectangular]",
    control_development_mode = FALSE,
    control_run_checks_only = FALSE,
    control_silence_progress = SILENT_PROGRESS,
    control_append_long_thin = FALSE,
    query_path = SAVE_SCRIPTS
  )
  
  # fetch actual
  actual_table = dbplyr.helpers::create_access_point(db_conn, tbl_name = "[tmp_long]")
  actual_table = dplyr::collect(actual_table)
  
  # fetch expected
  expected_table = read.csv(file.path(path, "output_long_thin.csv"), stringsAsFactors = FALSE)
  expected_table = dplyr::filter(expected_table, .data$should.appear.in.SQL.table == "YES")
  expected_table = dplyr::select(expected_table, -"from.row.number.in.input1", -"from.row.number.in.input2", -"should.appear.in.SQL.table")
  
  # make consistent
  actual_table$label_summary_period = dbplyr.helpers:::remove_delimiters(actual_table$label_summary_period, "\"")
  actual_table$label_measure = dbplyr.helpers:::remove_delimiters(actual_table$label_measure, "\"")
  actual_table$label_measure = gsub("=\"", "=", actual_table$label_measure)
  
  expected_table$label_identity = dbplyr.helpers:::remove_delimiters(expected_table$label_identity, "\"")
  expected_table$label_summary_period = dbplyr.helpers:::remove_delimiters(expected_table$label_summary_period, "\"")
  expected_table$label_measure = dbplyr.helpers:::remove_delimiters(expected_table$label_measure, "\"")
  expected_table$label_measure = gsub("\"=\"", "=", expected_table$label_measure)
  
  # assert
  expect_true(all(colnames(actual_table) %in% colnames(expected_table)))
  expect_true(all(colnames(expected_table) %in% colnames(actual_table)))
  
  value_compare = dplyr::full_join(
    actual_table,
    expected_table,
    by = c("identity_column", "label_identity", "summary_period_start_date", "summary_period_end_date", "label_summary_period", "label_measure"),
    suffix = c("_a", "_e")
  )
  
  expect_true(nrow(value_compare) == nrow(expected_table))
  expect_true(ncol(value_compare) == (1 + ncol(expected_table)))
  
  expect_false(any(is.na(actual_table$value_measure)))
  expect_false(any(is.na(expected_table$value_measure)))
  expect_true(all(abs(value_compare$value_measure_a - value_compare$value_measure_e) < 0.01))
})

## prepare_rectangular_table ---------------------------------------------- ----

test_that("rectaungular table produced", {
  
  prepare_rectangular_table(
    population_control_table = control_population,
    measures_control_table = control_measure,
    db_connection = db_conn,
    output_database = "[]",
    output_schema = "[]",
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rectangular]",
    control_development_mode = FALSE,
    control_run_checks_only = FALSE,
    control_silence_progress = SILENT_PROGRESS,
    control_append_long_thin = FALSE,
    query_path = SAVE_SCRIPTS
  )
  
  # fetch actual
  actual_table = dbplyr.helpers::create_access_point(db_conn, tbl_name = "[tmp_rectangular]")
  actual_table = dplyr::collect(actual_table)
  
  # fetch expected
  expected_table = read.csv(file.path(path, "output_rectangular.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  
  # sort
  actual_table = dplyr::arrange(actual_table, identity_column, label_identity)
  expected_table = dplyr::arrange(expected_table, identity_column, label_identity)
  
  # assert
  expect_true(all(colnames(actual_table) %in% colnames(expected_table)))
  expect_true(all(colnames(expected_table) %in% colnames(actual_table)))
  expect_true(nrow(actual_table) == nrow(expected_table))
  
  for(col in colnames(actual_table)){
    
    if(is.numeric(actual_table[[col]])){
      expect_true(all(abs(actual_table[[col]] - expected_table[[col]]) < 0.01))
      
    } else {
      expect_true(all(actual_table[[col]] == expected_table[[col]]))
      
    }
  }
})

## dataset_assembly_tool 2 ------------------------------------------------ ----

test_that("full assembly works", {
  
  dataset_assembly_tool(
    population_control_table = control_population,
    measures_control_table = control_measure,
    db_connection = db_conn,
    output_database = "[]",
    output_schema = "[]",
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rectangular]",
    control_development_mode = FALSE,
    control_run_checks_only = FALSE,
    control_silence_progress = SILENT_PROGRESS,
    control_append_long_thin = FALSE,
    query_path = SAVE_SCRIPTS
  )
  
  #### validate long-thin ----
  
  # fetch actual
  actual_table = dbplyr.helpers::create_access_point(db_conn, tbl_name = "[tmp_long]")
  actual_table = dplyr::collect(actual_table)
  
  # fetch expected
  expected_table = read.csv(file.path(path, "output_long_thin.csv"), stringsAsFactors = FALSE)
  expected_table = dplyr::filter(expected_table, .data$should.appear.in.SQL.table == "YES")
  expected_table = dplyr::select(expected_table, -"from.row.number.in.input1", -"from.row.number.in.input2", -"should.appear.in.SQL.table")
  
  # make consistent
  actual_table$label_summary_period = dbplyr.helpers:::remove_delimiters(actual_table$label_summary_period, "\"")
  actual_table$label_measure = dbplyr.helpers:::remove_delimiters(actual_table$label_measure, "\"")
  actual_table$label_measure = gsub("=\"", "=", actual_table$label_measure)
  
  expected_table$label_identity = dbplyr.helpers:::remove_delimiters(expected_table$label_identity, "\"")
  expected_table$label_summary_period = dbplyr.helpers:::remove_delimiters(expected_table$label_summary_period, "\"")
  expected_table$label_measure = dbplyr.helpers:::remove_delimiters(expected_table$label_measure, "\"")
  expected_table$label_measure = gsub("\"=\"", "=", expected_table$label_measure)
  
  # assert
  expect_true(nrow(actual_table) == nrow(expected_table))
  expect_true(ncol(actual_table) == ncol(expected_table))
  
  expect_true(all(colnames(actual_table) %in% colnames(expected_table)))
  expect_true(all(colnames(expected_table) %in% colnames(actual_table)))
  
  for(col in colnames(actual_table)){
    
    if(is.numeric(actual_table[[col]])){
      expect_true(all(abs(sort(actual_table[[col]]) - sort(expected_table[[col]])) < 0.01))
      
    } else {
      expect_true(all(sort(actual_table[[col]]) == sort(expected_table[[col]])))
      
    }
  }
  
  #### validate rectangular ----
  
  # fetch actual
  actual_table = dbplyr.helpers::create_access_point(db_conn, tbl_name = "[tmp_rectangular]")
  actual_table = dplyr::collect(actual_table)
  
  # fetch expected
  expected_table = read.csv(file.path(path, "output_rectangular.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  
  # assert
  expect_true(nrow(actual_table) == nrow(expected_table))
  expect_true(ncol(actual_table) == ncol(expected_table))
  
  expect_true(all(colnames(actual_table) %in% colnames(expected_table)))
  expect_true(all(colnames(expected_table) %in% colnames(actual_table)))
  
  for(col in colnames(actual_table)){
    
    if(is.numeric(actual_table[[col]])){
      expect_true(all(abs(sort(actual_table[[col]]) - sort(expected_table[[col]])) < 0.01))
      
    } else {
      expect_true(all(sort(actual_table[[col]]) == sort(expected_table[[col]])))
      
    }
  }
})
