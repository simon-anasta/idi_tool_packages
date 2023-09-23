###############################################################################
#' Description: Data assembly tool
#'
#' Input: Two control files (1) population definition, (2) measures for analysis
#'
#' Output: Summarised measures for each population
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: utility_functions.R, general_assembly_tool_functions.R already sourced
#'
#' Notes:
#' - Uses code folding by headers (Alt + O to collapse all)
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-11-18 SA v2 for release
#' 2020-01-06 SA v1
#' 2019-11-26 SA v0
#' #############################################################################



## master function --------------------------------------------------------------------------------
#' Dataset assembly tool master function
#' 
#' 
#' 
#' 
#' @param population_control_table 
#' @param measures_control_table 
#' @param db_connection
#' @param output_database 
#' @param output_schema 
#' @param output_table 
#' @param control_development_mode 
#' @param control_run_checks_only 
#' @param control_silence_progress 
#' @param control_append_long_thin 
#' 
#' @return
#' 
#' @export
#' 
dataset_assembly_tool = function(
    population_control_table,
    measures_control_table,
    db_connection,
    output_database,
    output_schema,
    output_table,
    control_development_mode,
    control_run_checks_only = FALSE,
    control_silence_progress = FALSE,
    control_append_long_thin = FALSE,
) {
  #### check inputs ----
  stopifnot(is.logical(control_development_mode))
  stopifnot(is.logical(control_run_checks_only))
  stopifnot(is.logical(control_silence_progress))
  stopifnot(is.logical(control_append_long_thin))
  stopifnot(is.character(output_database))
  stopifnot(is.character(output_schema))
  stopifnot(is.character(output_table))
  dbplyr.helpers:::warn_if_missing_delimiters(output_database, output_schema, output_table)
  
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
    assemble_output_table(
      population_control_table, measures_control_table,
      output_database, output_schema, output_table,
      control_development_mode, control_overwrite_output_table, control_verbose
    )
    run_time_inform_user("output table created", context = "heading", print_level = control_verbose)
  }

  #### finish ----
  run_time_inform_user("general data assembly tool ended", context = "heading", print_level = control_verbose)
}


## create output table ----------------------------------------------------------------------------
#'
#' Create the new table and populate it. Records are appended
#' to the new table for every combination of population and
#' measure.
#'
assemble_output_table = function(
    population_table,
    measure_table,
    output_database, 
    output_schema, 
    output_table,
    control_development_mode, 
    control_overwrite_output_table, 
    control_verbose
) {
  #### existence of output table ----
  # connect to db
  db_con = create_database_connection(database = output_database)

  # delete
  if (control_overwrite_output_table) {
    delete_table(db_con, output_database, output_schema, output_table)
  }
  # required table
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
  if (!table_or_view_exists_in_db(db_con, output_database, output_schema, output_table)) {
    run_time_inform_user("creating table", context = "all", print_level = control_verbose)
    create_table(db_con, output_database, output_schema, output_table, output_columns, OVERWRITE = FALSE)
  }
  # confirm table has required columns
  out_tbl = create_access_point(db_con, output_database, output_schema, output_table)
  assert(table_contains_required_columns(out_tbl, names(output_columns), only = TRUE), "output table missing column")
  close_database_connection(db_con)
  run_time_inform_user("existence of output table verified", context = "details", print_level = control_verbose)

  #### access and append values ----

  # for each row in population table
  for (row_p in 1:nrow(population_table)) {
    # values
    p_identity_column = prep_for_sql(population_table[[row_p, "identity_column"]], alias = "p")
    p_identity_label = prep_for_sql(population_table[[row_p, "label_identity"]], alias = "p")
    p_start_date = prep_for_sql(population_table[[row_p, "summary_period_start_date"]], alias = "p")
    p_end_date = prep_for_sql(population_table[[row_p, "summary_period_end_date"]], alias = "p")
    p_period_label = prep_for_sql(population_table[[row_p, "label_summary_period"]], alias = "p")

    # for each row in measure table
    for (row_m in 1:nrow(measure_table)) {
      # values
      m_identity_column = prep_for_sql(measure_table[[row_m, "identity_column"]], alias = "m")
      m_start_date = prep_for_sql(measure_table[[row_m, "measure_period_start_date"]], alias = "m")
      m_end_date = prep_for_sql(measure_table[[row_m, "measure_period_end_date"]], alias = "m")
      m_label = prep_for_sql(measure_table[[row_m, "label_measure"]], alias = "m")
      m_value = prep_for_sql(measure_table[[row_m, "value_measure"]], alias = "m")

      # connect
      db_con = create_database_connection(database = output_database)

      # components
      from_population = sprintf(
        "%s.%s.%s",
        population_table[[row_p, "database_name"]],
        population_table[[row_p, "schema_name"]],
        population_table[[row_p, "table_name"]]
      )
      from_measure = sprintf(
        "%s.%s.%s",
        measure_table[[row_m, "database_name"]],
        measure_table[[row_m, "schema_name"]],
        measure_table[[row_m, "table_name"]]
      )
      optional_top = ifelse(control_development_mode, " TOP 1000 ", " ")

      calculation = handle_summary_case(
        summary_type = measure_table[[row_m, "measure_summarised_by"]],
        proportional = as.logical(measure_table[[row_m, "proportional"]]),
        m_label, m_value,
        m_start_date, m_end_date, p_start_date, p_end_date
      )

      group_by_columns = c(p_identity_column, p_identity_label, p_start_date, p_end_date, p_period_label, calculation$group)
      group_by_columns = group_by_columns[!is_delimited(group_by_columns, "'")]
      GROUP_BY = ifelse(length(group_by_columns) == 0, "", paste0("GROUP BY ", paste0(group_by_columns, collapse = ", ")))

      # prepare query
      sql_query = dbplyr::build_sql(
        con = db_con,
        sql(glue::glue(
          "SELECT {optional_top}\n",
          "       {p_identity_column} AS [identity_column]\n",
          "      ,{p_identity_label}  AS [label_identity]\n",
          "      ,{p_start_date} AS [summary_period_start_date]\n",
          "      ,{p_end_date}   AS [summary_period_end_date]\n",
          "      ,{p_period_label}  AS [label_summary_period]\n",
          "      ,{calculation$label} AS [label_measure]\n",
          "      ,{calculation$value} AS [value_measure]\n",
          "FROM {from_population} AS p\n",
          "INNER JOIN {from_measure} AS m\n",
          "ON {p_identity_column} = {m_identity_column}\n",
          "AND {p_start_date} <= {m_end_date}\n",
          "AND {m_start_date} <= {p_end_date}\n",
          "WHERE {calculation$label} IS NOT NULL\n",
          "{GROUP_BY}"
        ))
      )
      table_to_append = dplyr::tbl(db_con, dbplyr::sql(sql_query))

      # append & conclude
      append_database_table(db_con, output_database, output_schema, output_table,
        list_of_columns = names(output_columns), table_to_append
      )
      close_database_connection(db_con)
      run_time_inform_user(sprintf(
        "completed population %3d of %3d, measure %4d of %4d",
        row_p, nrow(population_table), row_m, nrow(measure_table)
      ),
      context = "details", print_level = control_verbose
      )
    }
  }

  #### tidy up ----
  #
  # compress and index not done on long-thin table as can interfere with appending.
}
