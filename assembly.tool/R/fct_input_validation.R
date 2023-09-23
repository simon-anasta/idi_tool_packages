################################################################################
# Description: Input validation functions for assembly tool
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## validate population and period control table --------------------------- ----
#' Validate population and period control table
#' 
#' Prior to executing the assembly tool, we validate all the inputs to minimize
#' the chance that the tool fails during data manipulation.
#' 
#' Check the population control table, with the checks that are shared with the
#' measure control table.
#' 
#' @param population_table the population control table to validate
#' 
#' @return T/F whether any check has been failed
#' 
validate_populaiton_control_table = function(population_table) {
  stopifnot(is.data.frame(population_table))
  
  # acceptable and default values
  column_reqs = data.frame(
    Database_name              = c("[sql] only", TRUE, FALSE),
    Schema_name                = c("[sql] only", TRUE, FALSE),
    Table_name                 = c("[sql] only", TRUE, FALSE),
    Identity_column            = c("[sql] or \"txt\"", TRUE, TRUE),
    Label_Identity             = c("[sql] or \"txt\"", TRUE, TRUE),
    Summary_period_start_date  = c("[sql] or \"txt\"", TRUE, TRUE),
    Summary_period_end_date    = c("[sql] or \"txt\"", TRUE, TRUE),
    Label_summary_period       = c("[sql] or \"txt\"", TRUE, TRUE),
    stringsAsFactors = FALSE,
    row.names = c("accepts", "sql", "txt")
  )
  
  any_check_failed = validate_control_table(population_table, "population", column_reqs)
  return(any_check_failed)
}

## validate measure control table 1 --------------------------------------- ----
#' Validate measure control table 1
#' 
#' Prior to executing the assembly tool, we validate all the inputs to minimize
#' the chance that the tool fails during data manipulation.
#' 
#' Check the measure control table, with the checks that are shared with the
#' population control table.
#' 
#' @param measure_table the measure control table to validate
#' 
#' @return T/F whether any check has been failed
#' 
validate_measure_control_table_1 = function(measure_table) {
  stopifnot(is.data.frame(measure_table))
  
  # acceptable and default values
  column_reqs = data.frame(
    Database_name              = c("[sql] only", TRUE, FALSE),
    Schema_name                = c("[sql] only", TRUE, FALSE),
    Table_name                 = c("[sql] only", TRUE, FALSE),
    Identity_column            = c("[sql] or \"txt\"", TRUE, TRUE),
    Measure_period_start_date  = c("[sql] or \"txt\"", TRUE, TRUE),
    Measure_period_end_date    = c("[sql] or \"txt\"", TRUE, TRUE),
    Label_measure              = c("[sql] or \"txt\"", TRUE, TRUE),
    Value_measure              = c("[sql] or \"txt\"", TRUE, TRUE),
    stringsAsFactors = FALSE,
    row.names = c("accepts", "sql", "txt")
  )
  
  any_check_failed = validate_control_table(measure_table, "measure", column_reqs)
  return(any_check_failed)
}

## validate control table - shared checks --------------------------------- ----
#' Validate control table - shared validation
#' 
#' Prior to executing the assembly tool, we validate all the inputs to minimize
#' the chance that the tool fails during data manipulation.
#' 
#' This function does the shared checks for both population and measure control
#' tables: required columns, no missing values, correct delimiters, no internal
#' delimiters.
#' 
#' @param control_table the control table to validate
#' @param type a string to indicate source control file. Either population or
#' measure.
#' @param column_reqs a data.frame that contains the column requirements for
#' the table to check.
#' 
#' @return T/F whether any check has been failed
#' 
validate_control_table = function(control_table, type, column_reqs) {
  stopifnot(is.data.frame(control_table))
  stopifnot(is.character(type))
  stopifnot(type %in% c("population", "measure"))
  stopifnot(is.data.frame(column_reqs))
  
  # prep
  any_check_failed = FALSE
  colnames(column_reqs) = tolower(colnames(column_reqs))
  colnames(control_table) = tolower(colnames(control_table))
  
  for (col in colnames(column_reqs)) {
    
    ## required columns
    if(!col %in% colnames(control_table)){
      any_check_failed = TRUE
      warning(glue::glue("column {col} missing from {type} input table"))
      next # column
    }
    
    for (row in 1:nrow(control_table)) {
      
      this_value = control_table[[row, col]]
      
      # checks
      not_na = !is.na(this_value)
      accepts_sql = as.logical(column_reqs["sql", col])
      accepts_txt = as.logical(column_reqs["txt", col])
      sql_delim = dbplyr.helpers:::is_delimited(this_value, "[]")
      txt_delim = dbplyr.helpers:::is_delimited(this_value, "\"")
      sql_pass = not_na && accepts_sql && sql_delim
      txt_pass = not_na && accepts_txt && txt_delim
      
      ## non-missing entries
      if (!not_na) {
        any_check_failed = TRUE
        warning(glue::glue("missing input in {type} control: column {col}, row {row}"))
        
        ## correct delimiters
      } else if (!sql_pass && !txt_pass) {
        any_check_failed = TRUE
        acceptable_values = column_reqs["accepts", col]
        warning(glue::glue(
          "unaccepted input in {type} control: column {col}, row {row}",
          " - acceptable values are {acceptable_values}"
        ))
      }
      
      ## no internal delimiters
      if(dbplyr.helpers:::has_internal_delimiters(this_value)){
        any_check_failed = TRUE
        warning(glue::glue(
          "unaccepted input in {type} control: column {col}, row {row}",
          " - internal delimiters detected"
        ))
      }
      
    } # end row iteration
  } # end column iteration
  
  return(any_check_failed)
}

## validate measure control table 2 --------------------------------------- ----
#' Validate measure control table 2
#' 
#' Prior to executing the assembly tool, we validate all the inputs to minimize
#' the chance that the tool fails during data manipulation.
#' 
#' Check the measure control table, with the checks that are specific to this
#' control table:
#' 
#' @param measure_table the measure control table to validate
#' 
#' @return T/F whether any check has been failed
#' 
validate_measure_control_table_2 = function(measure_table) {
  stopifnot(is.data.frame(measure_table))
  
  # acceptable and default values
  column_reqs = list(
    Measure_summarised_by = c("SUM", "COUNT", "EXISTS", "MIN", "MAX", "DURATION", "HISTOGRAM", "DISTINCT", "MEAN"),
    Proportional = c("true", "t", "false", "f")
  )
  
  # prep
  any_check_failed = FALSE
  names(column_reqs) = tolower(names(column_reqs))
  colnames(measure_table) = tolower(colnames(measure_table))
  
  for (col in names(column_reqs)) {
    
    ## required columns
    if(!col %in% colnames(measure_table)){
      any_check_failed = TRUE
      warning(glue::glue("column {col} missing from measure input table"))
      next # column
    }
    
    for (row in 1:nrow(measure_table)) {
      
      this_value = measure_table[[row, col]]
      
      # checks
      not_na = !is.na(this_value)
      accepted = tolower(this_value) %in% tolower(column_reqs[[col]])
      pass = not_na && accepted
      
      ## non-missing entries
      if (!not_na) {
        any_check_failed = TRUE
        warning(glue::glue("missing input in measure control: column {col}, row {row}"))
        
        ## correct contents
      } else if (!pass) {
        any_check_failed = TRUE
        acceptable_values = paste0(column_reqs[[col]], collapse = ",")
        warning(glue::glue(
          "unaccepted input in measure control: column {col}, row {row}",
          " - acceptable values are {acceptable_values}"
        ))
      }
      
    } # end row iteration
  } # end column iteration
  
  return(any_check_failed)
}

## validate database tables ----------------------------------------------- ----
#' Validate database tables
#' 
#' Check that all tables requested in control tables exist
#' and that their required columns exist
#' 
#' @param control_table a control table to validate
#' @param db_connection a connection to the database to validate against
#'
#' @return T/F whether any check has been failed
#' 
validate_database_tables = function(control_table, db_connection) {
  stopifnot(is.data.frame(control_table))
  
  # prep
  any_check_failed = FALSE
  col_names = colnames(control_table)
  col_names = col_names[!col_names %in% c("database_name", "schema_name", "table_name")]
  
  for(row in 1:nrow(control_table)){
    
    # can connect to database
    this_db = control_table[[ii, "database_name"]]
    this_schema = control_table[[ii, "schema_name"]]
    this_table = control_table[[ii, "table_name"]]
    tmp_table = NA
    
    try(
      tmp_table = create_access_point(db_connection, this_db, this_schema, this_table),
      silent = TRUE
    )
    
    if(is.na(tmp_table)){
      any_check_failed = TRUE
      warning(glue::glue("table {this_db}.{this_schema}.{this_table} could not be accessed"))
      next # row
    }
    
    for (col in col_names) {
      
      this_column = control_table[[row, col]]
      
      # skip constants / non-sql columns
      if(!dbplyr.helpers:::is_delimited(this_column, "[]")){
        next
      }
      
      this_column = remove_delimiters(this_column, "[]")
      
      # column is in table
      if(! this_column %in% colnames(tmp_table)){
        any_check_failed = TRUE
        warning(glue::glue("column {this_column} missing from table {this_table}"))
      }
      
      # next iteration if data type does not need checking
      if(col != "value_measure"){
        next
      }
      summary_type = control_table[[ii, "measure_summarised_by"]]
      if(! toupper(summary_type) %in% c("MIN", "MAX", "SUM", "MEAN")){
        next
      }
      # fetch data example
      tmp_table = dplyr::filter(tmp_table, !is.na(!! rlang::sym(this_column)))
      tmp_table_head = dplyr::collect(utils::head(tmp_table))
      example_values = tmp_table_head[[this_column]]
      
      # check data type
      if(!is.numeric(example_values) & !is.logical(example_values)){
        any_check_failed = TRUE
        warning(glue::glue("Can not {summary_type} column {this_column} of table {this_table} as it is not numeric"))
      }
    
    } # end column iteration
  } # end row iteration

  return(any_check_failed)
}
