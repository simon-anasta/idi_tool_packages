################################################################################
# Description: Core execution of the assembly tool
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## master function -------------------------------------------------------- ----
#' Dataset assembly tool master function
#' 
#' Run the Dataset Assembly Tool:
#' * Reads and validates the contents of both control files
#' * Assembles a long-thin table according to control files
#' * Pivots the long-thin file to rectangular, research-ready format
#' 
#' See the users' manual for detailed instructions.
#' 
#' @param population_control_table Either the file path to the population and
#' period control file (in a csv or Excel format) or a data.frame containing
#' the contents of the control file.
#' @param measures_control_table Either the file path to the measures
#' control file (in a csv or Excel format) or a data.frame containing
#' the contents of the control file.
#' @param db_connection An open connection to the database that contains the
#' input tables (as specified in the control files) and the output location.
#' @param output_database The name of the database that the output should be
#' saved to. You mist have write permissions to this location.
#' Ideally delimited in square brackets.
#' @param output_schema The name of the schema that the output should be saved
#' to. Use a placeholder value if the database does not permit schemas. You
#' must have write permissions to this location.
#' Ideally delimited in square brackets.
#' @param output_table_long The name of the long-thin table that the output
#' should be saved into. Will be overwritten unless `control_append_long_thin`
#' is set to TRUE. Ideally delimited in square brackets.
#' @param output_table_rectangular The name of the rectangular, research-ready
#' table that the output should be saved into. Will be overwritten if it already
#' exists. Ideally delimited in square brackets.
#' @param control_development_mode T/F should the tool be run in development
#' mode. In development mode only the first 1000 records for each measure are
#' assembled. This allows for more rapid testing of correctness.
#' @param control_run_checks_only (Optional) T/F should the tool only validate
#' the control files and not run any database processing. Defaults to FALSE.
#' @param control_silence_progress (Optional) T/F should the tool avoid showing
#' progress information during execution. Defaults to FALSE.
#' @param control_append_long_thin (Optional) T/F should the tool append the new
#' records to the existing long-thin table. Useful if undertaking assembly in
#' multiple steps. Defaults to FALSE.
#' 
#' @return NULL
#' 
#' @export
#' 
dataset_assembly_tool = function(
    population_control_table,
    measures_control_table,
    db_connection,
    output_database,
    output_schema,
    output_table_long,
    output_table_rectangular,
    control_development_mode,
    control_run_checks_only = FALSE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE
) {
  #### check inputs ----
  stopifnot(is.logical(control_development_mode))
  stopifnot(is.logical(control_run_checks_only))
  stopifnot(is.logical(control_silence_progress))
  stopifnot(is.logical(control_append_long_thin))
  stopifnot(is.character(output_database))
  stopifnot(is.character(output_schema))
  stopifnot(is.character(output_table_long))
  dbplyr.helpers:::warn_if_missing_delimiters(output_database, output_schema, output_table_long)
  
  dbplyr.helpers::run_time_inform_user("dataset assembly tool begun", "heading", control_silence_progress)
  
  #### read and check control tables ----
  
  # load tables if file path provided
  if (is.character(population_control_table)) {
    tmp = NA
    try({tmp = read_table_file(population_control_table)}, silent = TRUE)
    if(is.na(tmp)){
      stop("Can not read population control file from disc")
    }
    population_control_table = tmp
  }
  
  if (is.character(measures_control_table)) {
    tmp = NA
    try({tmp = read_table_file(measures_control_table)}, silent = TRUE)
    if(is.na(tmp)){
      stop("Can not read measure control file from disc")
    }
    measures_control_table = tmp
  }

  # all control file contents is text
  population_control_table[] = lapply(population_control_table, as.character)
  measures_control_table[] = lapply(measures_control_table, as.character)
  
  # convert tbl to data.frame
  if (is.tbl(population_control_table)) {
    population_control_table = as.data.frame(population_control_table)
  }
  if (is.tbl(measures_control_table)) {
    measures_control_table = as.data.frame(measures_control_table)
  }
  stopifnot(is.data.frame(population_control_table))
  stopifnot(is.data.frame(measures_control_table))
  
  colnames(population_control_table) = tolower(colnames(population_control_table))
  colnames(measures_control_table) = tolower(colnames(measures_control_table))

  #### validate control tables ----
  
  checks1 = validate_population_control_table(population_control_table)
  dbplyr.helpers::run_time_inform_user("population and period table checked", "details", control_silence_progress)
  checks2 = validate_measure_control_table_1(measure_table)
  dbplyr.helpers::run_time_inform_user("measures and indicators (1 of 2) checked", "details", control_silence_progress)
  checks3 = validate_measure_control_table_2(measure_table)
  dbplyr.helpers::run_time_inform_user("measures and indicators (2 of 2) checked", "details", control_silence_progress)
  
  control_file_checks_all_pass = checks1 & checks2 & checks3
  stopifnot(control_file_checks_all_pass)
  dbplyr.helpers::run_time_inform_user("controls and inputs validated", "heading", control_silence_progress)
  
  #### narrow to required columns ----

  population_control_table = dplyr::select(
    population_control_table,  
    "database_name",
    "schema_name",
    "table_name",
    "identity_column",
    "label_identity",
    "summary_period_start_date",
    "summary_period_end_date",
    "label_summary_period"
  )
  measures_control_table = dplyr::select(
    measures_control_table, 
    "database_name",
    "schema_name",
    "table_name",
    "identity_column",
    "measure_period_start_date",
    "measure_period_end_date",
    "label_measure",
    "value_measure",
    "measure_summarised_by",
    "proportional"
  )
  
  #### validate database tables ----
  checks4 = validate_database_tables(population_control_table, db_connection)
  dbplyr.helpers::run_time_inform_user("database tables validated for population control", "details", control_silence_progress)
  checks5 = validate_database_tables(measures_control_table, db_connection)
  dbplyr.helpers::run_time_inform_user("database tables validated for measure control", "details", control_silence_progress)
  
  database_checks_all_pass = checks4 & checks5
  stopifnot(database_checks_all_pass)
  dbplyr.helpers::run_time_inform_user("database contents validated", "heading", control_silence_progress)
  
  #### create output table ----
  
  if (!control_run_checks_only) {
    # make long-thin
    assemble_output_table(
      population_control_table,
      measures_control_table,
      db_connection,
      output_database,
      output_schema,
      output_table_long,
      output_table_rectangular,
      control_development_mode,
      control_run_checks_only,
      control_silence_progress,
      control_append_long_thin
    )
    # notify user
    dbplyr.helpers::run_time_inform_user("long-thin table assembled", context = "heading", control_silence_progress)
    
    # make rectangular
    prepare_rectangular_table(
      population_control_table,
      measures_control_table,
      db_connection,
      output_database,
      output_schema,
      output_table_long,
      output_table_rectangular,
      control_development_mode,
      control_run_checks_only,
      control_silence_progress,
      control_append_long_thin
    )
    # notify user
    dbplyr.helpers::run_time_inform_user("rectangular table prepared", context = "heading", control_silence_progress)
    
  }

  #### finish ----
  dbplyr.helpers::run_time_inform_user("dataset assembly tool complete", "heading", control_silence_progress)
  return(NULL)
}

## assemble long-thin table ----------------------------------------------- ----
#' Assemble long-thin table
#' 
#' Sub-function of `dataset_assembly_tool`. Handles the creation and population
#' of the long-thin table.
#' 
#' Records are appended to the new table for every combination of population
#' and measure.
#' 
#' All inputs as per the master function.
#'
#' @param output_database 
#' @param output_schema 
#' @param population_control_table 
#' @param measures_control_table 
#' @param db_connection 
#' @param output_table_long 
#' @param output_table_rectangular 
#' @param control_run_checks_only 
#' @param control_silence_progress 
#' @param control_append_long_thin 
#' @param control_development_mode 
#'
#' @return NULL
#' 
assemble_output_table = function(
    population_control_table,
    measures_control_table,
    db_connection,
    output_database,
    output_schema,
    output_table_long,
    output_table_rectangular,
    control_development_mode,
    control_run_checks_only = FALSE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE  
) {
  #### output table ----
  
  # delete
  if (!control_append_long_thin) {
    dbplyr.helpers::run_time_inform_user("deleting existing long-thin table", "all", control_silence_progress)
    dbplyr.helpers::delete_table(db_connection, output_database, output_schema, output_table_long)
  }
  
  # required long-thin table
  output_columns = list(
    identity_column = "[int] NOT NULL",
    label_identity = "[varchar](50) NOT NULL",
    summary_period_start_date = "[date] NOT NULL",
    summary_period_end_date = "[date] NOT NULL",
    label_summary_period = "[varchar](50) NOT NULL",
    label_measure = "[varchar](70) NOT NULL",
    value_measure = "[FLOAT](53) NULL"
  )
  
  # create if does not exist
  if (!table_or_view_exists_in_db(db_connection, output_database, output_schema, output_table_long)) {
    dbplyr.helpers::run_time_inform_user("creating long-thin table", "all", control_silence_progress)
    dbplyr.helpers::create_table(db_connection, output_database, output_schema, output_table_long, output_columns, OVERWRITE = FALSE)
  }
  
  # confirm table has required columns
  out_tbl = dbplyr.helpers::create_access_point(db_connection, output_database, output_schema, output_table_long)
  stopifnot(dbplyr.helpers::table_contains_required_columns(out_tbl, names(output_columns), only = TRUE))
  dbplyr.helpers::run_time_inform_user("existence of output table verified", "details", control_silence_progress)
  
  #### query constants ----
  
  query_text = c(
    "SELECT {optional_top}\n",
    "       {p_identity_column} AS [identity_column]",
    "      ,{p_identity_label}  AS [label_identity]",
    "      ,{p_start_date} AS [summary_period_start_date]",
    "      ,{p_end_date}   AS [summary_period_end_date]",
    "      ,{p_period_label}  AS [label_summary_period]",
    "      ,{calculation$label} AS [label_measure]",
    "      ,{calculation$value} AS [value_measure]",
    "FROM {from_population} AS p",
    "INNER JOIN {from_measure} AS m",
    "ON {p_identity_column} = {m_identity_column}",
    "AND {p_start_date} <= {m_end_date}",
    "AND {m_start_date} <= {p_end_date}",
    "WHERE {calculation$label} IS NOT NULL",
    "{GROUP_BY}"
  )
  
  optional_top = ifelse(control_development_mode, " TOP 1000 ", " ")
  
  
  #### access and append values ----

  # for each row in population table
  for (row_p in 1:nrow(population_control_table)) {
    # values
    p_identity_column = prep_for_sql(population_control_table[[row_p, "identity_column"]], alias = "p")
    p_identity_label = prep_for_sql(population_control_table[[row_p, "label_identity"]], alias = "p")
    p_start_date = prep_for_sql(population_control_table[[row_p, "summary_period_start_date"]], alias = "p")
    p_end_date = prep_for_sql(population_control_table[[row_p, "summary_period_end_date"]], alias = "p")
    p_period_label = prep_for_sql(population_control_table[[row_p, "label_summary_period"]], alias = "p")

    # for each row in measure table
    for (row_m in 1:nrow(measures_control_table)) {
      # values
      m_identity_column = prep_for_sql(measures_control_table[[row_m, "identity_column"]], alias = "m")
      m_start_date = prep_for_sql(measures_control_table[[row_m, "measure_period_start_date"]], alias = "m")
      m_end_date = prep_for_sql(measures_control_table[[row_m, "measure_period_end_date"]], alias = "m")
      m_label = prep_for_sql(measures_control_table[[row_m, "label_measure"]], alias = "m")
      m_value = prep_for_sql(measures_control_table[[row_m, "value_measure"]], alias = "m")
      
      # components
      from_population = sprintf(
        "%s.%s.%s",
        population_control_table[[row_p, "database_name"]],
        population_control_table[[row_p, "schema_name"]],
        population_control_table[[row_p, "table_name"]]
      )
      from_measure = sprintf(
        "%s.%s.%s",
        measures_control_table[[row_m, "database_name"]],
        measures_control_table[[row_m, "schema_name"]],
        measures_control_table[[row_m, "table_name"]]
      )
      
      calculation = handle_summary_case(
        summary_type = measures_control_table[[row_m, "measure_summarised_by"]],
        proportional = as.logical(measures_control_table[[row_m, "proportional"]]),
        m_label, m_value,
        m_start_date, m_end_date, p_start_date, p_end_date
      )

      group_by_columns = c(p_identity_column, p_identity_label, p_start_date, p_end_date, p_period_label, calculation$group)
      group_by_columns = group_by_columns[!dbplyr.helpers::is_delimited(group_by_columns, "'")]
      GROUP_BY = ifelse(length(group_by_columns) == 0, "", paste0("GROUP BY ", paste0(group_by_columns, collapse = ", ")))

      # prepare query
      sql_query = dbplyr::build_sql(
        con = db_connection,
        sql(glue::glue(paste(query_text, collapse = "\n")))
      )
      table_to_append = dplyr::tbl(db_connection, dbplyr::sql(sql_query))

      # append & conclude
      append_database_table(
        table_to_append, 
        db_connection, 
        output_database, 
        output_schema, 
        output_table_long,
        list_of_columns = names(output_columns)
      )
      
      msg = sprintf(
        "completed population %3d of %3d, measure %4d of %4d",
        row_p, nrow(population_control_table), row_m, nrow(measures_control_table)
      )
      dbplyr.helpers::run_time_inform_user(msg, context = "details", control_silence_progress)
    }
  }
  
  #### conclude ----
  
  connection_is_sql_server = any(grepl("SQL Server", class(db_connection)))
  
  # compress & index assembled table
  if(connection_is_sql_server){
    dbplyr.helpers::run_time_inform_user("compacting long-thin table", "details", control_silence_progress)
    dbplyr.helpers::compress_table(db_connection, output_database, output_schema, output_table_long)
    dbplyr.helpers::run_time_inform_user("indexing long-thin table", "details", control_silence_progress)
    dbplyr.helpers::create_nonclustered_index(db_connection, output_database, output_schema, output_table_long, "identity_column")
  }
  
  return(NULL)
}

## prepare rectangular table ---------------------------------------------- ----
#' Prepare rectangular table
#' 
#' Sub-function of `dataset_assembly_tool`. Checks the long-thin table and
#' pivots to the rectangular, research-ready table.
#' 
#' All inputs as per the master function.
#' 
#' @param population_control_table 
#' @param measures_control_table 
#' @param db_connection 
#' @param output_database 
#' @param output_schema 
#' @param output_table_long 
#' @param output_table_rectangular 
#' @param control_development_mode 
#' @param control_run_checks_only 
#' @param control_silence_progress 
#' @param control_append_long_thin 
#' 
#' @return NULL
#' 
prepare_rectangular_table = function(
  population_control_table,
  measures_control_table,
  db_connection,
  output_database,
  output_schema,
  output_table_long,
  output_table_rectangular,
  control_development_mode,
  control_run_checks_only,
  control_silence_progress,
  control_append_long_thin
){
  #### warn if long-thin table contains duplicates ----
  dbplyr.helpers::run_time_inform_user("checking for duplicates", "details", control_silence_progress)
  
  column_combination_expecting_uniqueness = c(
    "identity_column", "label_identity",
    "summary_period_start_date", "summary_period_end_date",
    "label_summary_period", "label_measure"
  )
  
  long_thin_tbl = dbplyr.helpers::create_access_point(db_connection, output_database, output_schema, output_table_long)
  any_duplicates = !check.dataset::check_all_unique(long_thin_tbl, column_combination_expecting_uniqueness)
  
  if (any_duplicates) {
    warning(
      "Duplicate outputs detected in long-thin table. Unexpected output likely.\n",
      "Check input population for duplicates identities.\n",
      "Check measure and summary period labels for duplicates.\n"
    )
  }
  
  #### pivot to create rectangular research ready table ----
  
  long_thin_table = dbplyr.helpers::create_access_point(db_connection, output_database, output_schema, output_table_long)
  pivoted_table = dbplyr.helpers::pivot_table(long_thin_table, label_column = "label_measure", value_column = "value_measure")
  
  dbplyr.helpers::run_time_inform_user("pivoting and saving rectangular", "heading", control_silence_progress)
  written_tbl = dbplyr.helpers::write_to_database(pivoted_table, db_connection, output_database, output_schema, output_table_rectangular, OVERWRITE = TRUE)
  
  #### conclude ----
  
  connection_is_sql_server = any(grepl("SQL Server", class(db_connection)))
  
  # compress & index assembled table
  if(connection_is_sql_server){
    dbplyr.helpers::run_time_inform_user("compacting rectangular table", "details", control_silence_progress)
    dbplyr.helpers::compress_table(db_connection, output_database, output_schema, output_table_rectangular)
    dbplyr.helpers::run_time_inform_user("indexing rectangular table", "details", control_silence_progress)
    dbplyr.helpers::create_nonclustered_index(db_connection, output_database, output_schema, output_table_rectangular, "identity_column")
  }
  
  return(NULL)
}