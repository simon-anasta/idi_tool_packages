################################################################################
# Description: Automated tests for general assembly tool.
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Testing the following functions
#    - validate_database_tables
#    - assemble_output_table
#    - prepare_rectangular_table
#    - dataset_assembly_tool
#
# Issues:
#
################################################################################

## setup connection ------------------------------------------------------- ----

db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
on.exit(DBI::dbDisconnect(db_conn))

# once correctness of component functions is confirmed
# it is sufficient to just validate length of output
NUM_LONG_ROWS = 53
NUM_RECT_ROWS = 4

## copy data into database ------------------------------------------------ ----

path = system.file("extdata", "SQL_server_tests", package = "assembly.tool")
data_accidents = read.csv(file.path(path, "data_accidents.csv"), stringsAsFactors = FALSE)
data_benefit_payment = read.csv(file.path(path, "data_benefit_payment.csv"), stringsAsFactors = FALSE)
data_project_population = read.csv(file.path(path, "data_project_population.csv"), stringsAsFactors = FALSE)

# remove quotes
data_accidents$accident_cause = gsub("[\"']", "", data_accidents$accident_cause)
data_project_population$appointment_type = gsub("[\"']", "", data_project_population$appointment_type)

dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, "[tmp_accidents]", data_accidents, OVERWRITE = TRUE, query_path = query_path)
dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, "[tmp_benefit_payment]", data_benefit_payment, OVERWRITE = TRUE, query_path = query_path)
dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, "[tmp_project_population]", data_project_population, OVERWRITE = TRUE, query_path = query_path)

## prepare control file inputs -------------------------------------------- ----

path = system.file("extdata", "SQL_server_tests", package = "assembly.tool")

control_population = read.csv(file.path(path, "control_population_and_period.csv"), stringsAsFactors = FALSE)
control_population[] = lapply(control_population, as.character)
colnames(control_population) = tolower(colnames(control_population))
control_population[, "schema_name"] = our_schema
control_population[, "database_name"] = table_db

control_measure = read.csv(file.path(path, "control_measures.csv"), stringsAsFactors = FALSE)
control_measure[] = lapply(control_measure, as.character)
colnames(control_measure) = tolower(colnames(control_measure))
control_measure[, "schema_name"] = our_schema
control_measure[, "database_name"] = table_db

## testing validate_database_tables function ------------------------------ ----

testthat::test_that("validation against database accepts correct input", {
  testthat::expect_false(assembly.tool::validate_database_tables(control_population, db_conn))
  testthat::expect_false(assembly.tool::validate_database_tables(control_measure, db_conn))
  
  testthat::expect_silent(assembly.tool::validate_database_tables(control_population, db_conn))
  testthat::expect_silent(assembly.tool::validate_database_tables(control_measure, db_conn))
})

testthat::test_that("validation against database rejects incorrect input", {
  # break input tables
  tmp_p = control_population; tmp_p[1, 1] = "[db that does not exist]"
  tmp_m = control_measure; tmp_m[1, 5] = "[col that does not exist]"
  
  testthat::expect_true(suppressWarnings(assembly.tool::validate_database_tables(tmp_p, db_conn)))
  testthat::expect_true(suppressWarnings(assembly.tool::validate_database_tables(tmp_m, db_conn)))
  
  testthat::expect_warning(assembly.tool::validate_database_tables(tmp_p, db_conn))
  testthat::expect_warning(assembly.tool::validate_database_tables(tmp_m, db_conn))
})

testthat::test_that("numeric operations fail on non-numeric columns", {
  data_benefit_payment_flaw = read.csv(file.path(path, "data_benefit_payment.csv"), stringsAsFactors = FALSE)
  data_benefit_payment_flaw$payments = as.character(data_benefit_payment_flaw$payments)
  
  dbplyr.helpers::copy_r_to_sql(db_conn, table_db, our_schema, "[tmp_benefit_payment_flaw]", data_benefit_payment_flaw, OVERWRITE = TRUE, query_path = query_path)
  
  # point measure table to flawed table
  tmp_m = control_measure
  tmp_m[tmp_m$table_name == "[tmp_benefit_payment]", "table_name"] = "[tmp_benefit_payment_flaw]"

  testthat::expect_true(suppressWarnings(assembly.tool::validate_database_tables(tmp_m, db_conn)))
  testthat::expect_warning(assembly.tool::validate_database_tables(tmp_m, db_conn))
  
  # tidy up
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_benefit_payment_flaw]", query_path = query_path)
})

## testing long-thin function --------------------------------------------- ----

testthat::test_that("long-thin table prepared as expected", {
  # make output
  testthat::expect_output(
    assembly.tool::assemble_output_table(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = table_db,
      output_schema = our_schema,
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rect]",
      control_development_mode = FALSE,
      control_run_checks_only = FALSE,
      control_silence_progress = FALSE,
      control_append_long_thin = FALSE,
      query_path = query_path
    )
  )
  
  testthat::expect_silent(
    assembly.tool::assemble_output_table(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = table_db,
      output_schema = our_schema,
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rect]",
      control_development_mode = FALSE,
      control_run_checks_only = FALSE,
      control_silence_progress = TRUE,
      control_append_long_thin = FALSE,
      query_path = query_path
    )
  )
  
  # retrieve output table
  actual_long = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_long]")
  actual_long = dplyr::collect(actual_long)
  
  # output exists
  testthat::expect_true(nrow(actual_long) == NUM_LONG_ROWS)
  
  # retrieve expected table
  expected_long = read.csv(file.path(path, "output_long_thin.csv"), stringsAsFactors = FALSE)
  expected_long = dplyr::filter(expected_long, .data$should.appear.in.SQL.table == "YES")
  expected_long = dplyr::select(expected_long, -"from.row.number.in.input1", -"from.row.number.in.input2", -"should.appear.in.SQL.table")
  
  # make consistent
  actual_long$label_summary_period = dbplyr.helpers:::remove_delimiters(actual_long$label_summary_period, "\"")
  actual_long$label_measure = dbplyr.helpers:::remove_delimiters(actual_long$label_measure, "\"")
  actual_long$label_measure = gsub("=\"", "=", actual_long$label_measure)
  
  expected_long$label_identity = dbplyr.helpers:::remove_delimiters(expected_long$label_identity, "\"")
  expected_long$label_summary_period = dbplyr.helpers:::remove_delimiters(expected_long$label_summary_period, "\"")
  expected_long$label_measure = dbplyr.helpers:::remove_delimiters(expected_long$label_measure, "\"")
  expected_long$label_measure = gsub("\"=\"", "=", expected_long$label_measure)
  
  # assert
  testthat::expect_true(all(colnames(actual_long) %in% colnames(expected_long)))
  testthat::expect_true(all(colnames(expected_long) %in% colnames(actual_long)))
  
  value_compare = dplyr::full_join(
    actual_long,
    expected_long,
    by = c("identity_column", "label_identity", "summary_period_start_date", "summary_period_end_date", "label_summary_period", "label_measure"),
    suffix = c("_a", "_e")
  )
  
  testthat::expect_true(nrow(value_compare) == nrow(expected_long))
  testthat::expect_true(ncol(value_compare) == (1 + ncol(expected_long)))
  
  testthat::expect_false(any(is.na(actual_long$value_measure)))
  testthat::expect_false(any(is.na(expected_long$value_measure)))
  testthat::expect_true(all(abs(value_compare$value_measure_a - value_compare$value_measure_e) < 0.01))
})

## testing rectangular function ------------------------------------------- ----

testthat::test_that("rectangular table prepared as expected", {
  # make output
  testthat::expect_output(
    assembly.tool::prepare_rectangular_table(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = table_db,
      output_schema = our_schema,
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rect]",
      control_development_mode = FALSE,
      control_run_checks_only = FALSE,
      control_silence_progress = FALSE,
      control_append_long_thin = FALSE,
      query_path = query_path
    )
  )
  
  testthat::expect_silent(
    assembly.tool::prepare_rectangular_table(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = table_db,
      output_schema = our_schema,
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rect]",
      control_development_mode = FALSE,
      control_run_checks_only = FALSE,
      control_silence_progress = TRUE,
      control_append_long_thin = FALSE,
      query_path = query_path
    )
  )
  
  # retrieve output table
  actual_rect = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_rect]")
  actual_rect = dplyr::collect(actual_rect)
  
  # output exists
  testthat::expect_true(nrow(actual_rect) == NUM_RECT_ROWS)
  
  # fetch expected
  expected_rect = read.csv(file.path(path, "output_rectangular.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  
  # sort
  actual_rect = dplyr::arrange(actual_rect, identity_column, label_identity)
  expected_rect = dplyr::arrange(expected_rect, identity_column, label_identity)
  
  # assert
  testthat::expect_true(all(colnames(actual_rect) %in% colnames(expected_rect)))
  testthat::expect_true(all(colnames(expected_rect) %in% colnames(actual_rect)))
  testthat::expect_true(nrow(actual_rect) == nrow(expected_rect))
  
  for(col in colnames(actual_rect)){
    
    if(is.numeric(actual_rect[[col]])){
      testthat::expect_true(all(abs(actual_rect[[col]] - expected_rect[[col]]) < 0.01))
      
    } else {
      testthat::expect_true(all(actual_rect[[col]] == expected_rect[[col]]))
      
    }
  }
})

## integration base test -------------------------------------------------- ----

testhat::test_that("entire tool runs", {
  # reset_output
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_long]", query_path = query_path)
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_rect]", query_path = query_path)
  
  assembly.tool::dataset_assembly_tool(
    population_control_table = control_population,
    measures_control_table = control_measure,
    db_connection = db_conn,
    output_database = table_db,
    output_schema = our_schema,
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rect]",
    control_development_mode = FALSE,
    control_run_checks_only = FALSE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE,
    query_path = query_path
  )
  
  actual_long = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_long]")
  testhat::expect_true(check.dataset::num_row(actual_long) == NUM_LONG_ROWS)
  
  actual_rect = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_rect]")
  testhat::expect_true(check.dataset::num_row(actual_rect) == NUM_RECT_ROWS)
  
  #### rerun to confirm overwriting happens ----
  assembly.tool::dataset_assembly_tool(
    population_control_table = control_population,
    measures_control_table = control_measure,
    db_connection = db_conn,
    output_database = table_db,
    output_schema = our_schema,
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rect]",
    control_development_mode = FALSE,
    control_run_checks_only = FALSE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE,
    query_path = query_path
  )
  
  actual_long = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_long]")
  testhat::expect_true(check.dataset::num_row(actual_long) == NUM_LONG_ROWS)
  
  actual_rect = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_rect]")
  testhat::expect_true(check.dataset::num_row(actual_rect) == NUM_RECT_ROWS)
  
  #### rerun in append mode ----
  
  # warning for duplication in long-thin
  testthat::expect_warning(
    assembly.tool::dataset_assembly_tool(
      population_control_table = control_population,
      measures_control_table = control_measure,
      db_connection = db_conn,
      output_database = table_db,
      output_schema = our_schema,
      output_table_long = "[tmp_long]",
      output_table_rectangular = "[tmp_rect]",
      control_development_mode = FALSE,
      control_run_checks_only = FALSE,
      control_silence_progress = FALSE,
      control_append_long_thin = TRUE,
      query_path = query_path
    )
  )
  
  actual_long = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_long]")
  testhat::expect_true(check.dataset::num_row(actual_long) == 2*NUM_LONG_ROWS)
  
  actual_rect = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_rect]")
  testhat::expect_true(check.dataset::num_row(actual_rect) == NUM_RECT_ROWS)
})

## only checks no database changes ---------------------------------------- ----

test_that("only checks can be run", {
  # reset_output
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_long]", query_path = query_path)
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_rect]", query_path = query_path)
  
  assembly.tool::dataset_assembly_tool(
    population_control_table = control_population,
    measures_control_table = control_measure,
    db_connection = db_conn,
    output_database = table_db,
    output_schema = our_schema,
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rect]",
    control_development_mode = FALSE,
    control_run_checks_only = TRUE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE,
    query_path = query_path
  )
  
  testthat::expect_false(dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, "[tmp_long]"))
  testthat::expect_false(dbplyr.helpers::table_or_view_exists_in_db(db_conn, table_db, our_schema, "[tmp_rect]"))
})

## run from file ---------------------------------------------------------- ----


test_that("tool accepts string inputs", {
  # reset_output
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_long]", query_path = query_path)
  dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_rect]", query_path = query_path)
  
  # write control files as csv
  write.csv(control_population, file.path(path, "alt_control_p.csv"), row.names = FALSE)
  write.csv(control_measure, file.path(path, "alt_control_m.csv"), row.names = FALSE)
  
  
  assembly.tool::dataset_assembly_tool(
    population_control_table =  file.path(path, "alt_control_p.csv"),
    measures_control_table = file.path(path, "alt_control_m.csv"),
    db_connection = db_conn,
    output_database = table_db,
    output_schema = our_schema,
    output_table_long = "[tmp_long]",
    output_table_rectangular = "[tmp_rect]",
    control_development_mode = FALSE,
    control_run_checks_only = FALSE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE,
    query_path = query_path
  )
  
  actual_long = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_long]")
  testhat::expect_true(check.dataset::num_row(actual_long) == NUM_LONG_ROWS)
  
  actual_rect = dbplyr.helpers::create_access_point(db_conn, table_db, our_schema, "[tmp_rect]")
  testhat::expect_true(check.dataset::num_row(actual_rect) == NUM_RECT_ROWS)
  
  # delete test files
  unlink(file.path(path, "alt_control_p.csv"))
  unlink(file.path(path, "alt_control_m.csv"))
})

## delete tmp tables from database ---------------------------------------- ----

dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_accidents]", query_path = query_path)
dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_benefit_payment]", query_path = query_path)
dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_project_population]", query_path = query_path)
dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_long]", query_path = query_path)
dbplyr.helpers::delete_table(db_conn,  table_db, our_schema, "[tmp_rect]", query_path = query_path)
