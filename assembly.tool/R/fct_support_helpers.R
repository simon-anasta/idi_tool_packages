################################################################################
# Description: Support functions for assembly tool
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Read csv, xls or xlsx file input --------------------------------------- ----
#' Auto-detecting which file read command is needed
#' 
#' Used to support csv, xls, and xlsx formatted control files.
#'
#' @param file_name_and_path location of the file to read into R
#'
#' @return the file contents as a data.frame
#' 
read_table_file = function(file_name_and_path) {
  stopifnot(is.character(file_name_and_path))
  
  extension = tools::file_ext(file_name_and_path)
  
  if (extension == "xls") {
    file_contents = readxl::read_xls(file_name_and_path)
  } else if (extension == "xlsx") {
    file_contents = readxl::read_xlsx(file_name_and_path)
  } else if (extension == "csv") {
    file_contents = utils::read.csv(file_name_and_path, stringsAsFactors = FALSE)
  } else {
    stop(sprintf("unaccepted file extension: %s", extension))
  }
  
  return(as.data.frame(file_contents, stringsAsFactors = FALSE))
}

## Has internal delimiters ------------------------------------------------ ----
#' Checks if internal delimiters exist
#'  
#' Tool inputs are delimited in either [] or "". We require that these inputs
#' do not contain additional delimiters within the string. For example:
#' "text\"more text" is not permitted even though the internal delimiter is
#' escaped.
#' 
#' @param string the character string to check
#' 
#' @return T/F whether the input contains internal delimiters
#' 
has_internal_delimiters = function(string) {
  stopifnot(is.character(string))
  
  internal_delim = FALSE
  
  if (dbplyr.helpers:::is_delimited(string, "[]")) {
    internal_delim = dbplyr.helpers:::has_internal_delimiters(string, "[]")
  }
  
  if (dbplyr.helpers:::is_delimited(string, "\"")) {
    internal_delim = dbplyr.helpers:::has_internal_delimiters(string, "\"")
  }
  
  return(internal_delim)
}

## Prepare inputs for SQL ------------------------------------------------- ----
#' Prepare user input for inclusion in SQL
#' 
#' For text input, replaces double " quote delimited strings
#' with single ' quote delimited strings.
#' 
#' For SQL input, appends alias.
#' Leaves all other strings unchanged.
#' 
#' @param string character string to prepare
#' @param alias table alias to append if needed
#'
#' @return prepared character string
#' 
prep_for_sql = function(string, alias) {
  stopifnot(is.character(string))
  stopifnot(is.character(alias))
  
  if (dbplyr.helpers:::is_delimited(string, "[]")) {
    return(glue::glue("{alias}.{string}"))
  }
  
  if (!dbplyr.helpers:::is_delimited(string, "\"")) {
    return(string)
  }
  
  string = gsub("\"", "'", string)
  stopifnot(dbplyr.helpers:::is_delimited(string, "'"))
  
  return(string)
}

## Handle cases of summary function --------------------------------------- ----
#' Handle summary function cases
#' 
#' A range of summary functions can be chosen for the output
#' e.g. MIN, MAX, SUM, COUNT, EXISTS, DURATION, HISTOGRAM, DISTINCT, MEAN.
#'
#' Each of these needs to be translated into 3 outputs
#'  1. label - the text label for the result
#'  2. value - the numeric value of the result
#'  3. group - which columns the table needs to be grouped by
#' These are used to build the SQL code required for assembly.
#' 
#' @param summary_type one of the accepted summary keywords
#' @param proportional T/F indicator for whether partial overlaps should be
#' scaled down proportionately.
#' @param m_label the (source) label for the measure
#' @param m_value the (source) value for the measure
#' @param m_start_date the start date for the measure spell
#' @param m_end_date the end date for the measure spell
#' @param p_start_date the start date for the study period
#' @param p_end_date the end date for the study period
#'
#' @return a list containing three arguments: label, value, and group.
#' 
handle_summary_case = function(
    summary_type,
    proportional,
    m_label,
    m_value,
    m_start_date,
    m_end_date,
    p_start_date,
    p_end_date
) {
  # checks
  stopifnot(is.character(summary_type))
  stopifnot(is.character(m_label))
  stopifnot(is.character(m_value))
  stopifnot(is.character(m_start_date))
  stopifnot(is.character(m_end_date))
  stopifnot(is.character(p_start_date))
  stopifnot(is.character(p_end_date))
  stopifnot(is.logical(proportional))
  stopifnot(toupper(summary_type) %in% c("MIN", "MAX", "SUM", "COUNT", "EXISTS", "DURATION", "HISTOGRAM", "DISTINCT", "MEAN"))
  
  # proportion calculation
  numerator = glue::glue(
    "DATEDIFF(DAY,",
    "IIF({m_start_date} < {p_start_date}, {p_start_date}, {m_start_date}),",
    "IIF({m_end_date} < {p_end_date}, {m_end_date}, {p_end_date}))"
  )
  denominator = glue::glue("DATEDIFF(DAY, {m_start_date}, {m_end_date})")
  proportional_calc = glue::glue("1.0 * (1 + {numerator}) / (1 + {denominator})")
  
  # prep and set defaults
  summary_type = tolower(summary_type)
  label = m_label
  group = m_label
  prop = ifelse(proportional, as.character(proportional_calc), "1.0")
  
  # handle cases
  if (summary_type == "min") {
    value = glue::glue("MIN({prop} * {m_value})")
    
  } else if (summary_type == "max") {
    value = glue::glue("MAX({prop} * {m_value})")
    
  } else if (summary_type == "sum") {
    value = glue::glue("SUM({prop} * {m_value})")
    
  } else if (summary_type == "count") {
    prop_warn(proportional, summary_type)
    value = glue::glue("COUNT({m_value})")
    
  } else if (summary_type == "exists") {
    prop_warn(proportional, summary_type)
    value = glue::glue("IIF( COUNT({m_value}) >= 1, 1, 0)")
    
  } else if (summary_type == "duration") {
    if (proportional) {
      value = glue::glue("SUM(1 + {numerator})")
    } else {
      value = glue::glue("SUM(1 + DATEDIFF(DAY, {m_start_date}, {m_end_date}))")
    }
    
  } else if (summary_type == "histogram") {
    prop_warn(proportional, summary_type)
    label = glue::glue("CONCAT({m_label},'=',{m_value})")
    value = glue::glue("COUNT({m_value})")
    group = c(m_label, m_value)
    
  } else if (summary_type == "distinct") {
    prop_warn(proportional, summary_type)
    value = glue::glue("COUNT(DISTINCT {m_value})")
    
  } else if (summary_type == "mean") {
    value = glue::glue("AVG({prop} * {m_value})")
    
  } else {
    stop("unrecognised summary_type")
    
  }
  
  return(list(
    label = as.character(label),
    value = as.character(value),
    group = group
  ))
}

## Warn if proportional unused -------------------------------------------- ----
#' Warn if proportional unused
#' 
#' Some summary types can not use the proportional setting.
#' 
#' This function exists to simplify `handle_summary_case`
#' 
#' @param proportional T/F whether to warn
#' @param keyword the matching keyword to warn about
#'
#' @return Nothing, displays warning if proportional is true.
#' 
prop_warn = function(proportional, keyword) {
  if (proportional) {
    keyword = toupper(keyword)
    msg = glue::glue("proportional = TRUE ignored when using '{keyword}'")
    warning(as.character(msg))
  }
}
