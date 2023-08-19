################################################################################
# Description: Wrapper for explorer package
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Replicates some existing R functions, but these versions will also work on
#   remote tables as well as local tables.
#
# Issues:
#
################################################################################

## Filter to limited number of rows --------------------------------------- ----
#' Filter to a limited number of rows
#' 
#' Given a data.frame, filter it to the specified maximum number of rows.
#' Designed to work with both local and remote data.frames.
#' 
#' Expected use case is sampling data for exploration or to minimize run time
#' of calculations during development.
#' 
#' Logic:
#' - If no id_column is specified, then return the first rows
#' - If an id_column is specified, then select a random sample that will
#'   return approximately the limit number of rows. This is done via modulus
#'   of the ID number.
#' 
#' Assumes that id_column is independent of data. filter is likely to produce
#' non-representative results if id_column is not random with respect to data.
#'  
#' @param df a data.frame (local or remote)
#' @param row_limit maximum number of rows in the returned table
#' @param id_column the column of a numeric id to be used for filtering
#' (optional - see logic for behavior). This column is assumed to be numeric
#' and integer.
#' 
#' @return a data.frame (in local R memory) containing at most `row_limit` rows.
#' 
#' @export
#' 
filter_to_limited_number_of_rows = function(df, row_limit = 10000, id_column = NA){
  # checks
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.numeric(row_limit))
  stopifnot(is.na(id_column) || is.character(id_column))
  stopifnot(is.na(id_column) || id_column %in% colnames(df))
  
  # number of rows (from table_consistency_checks.R)
  number_rows = num_row(df)

  # filter if necessary
  if(number_rows > row_limit & !is.na(id_column)){
    mod_value = ceiling(number_rows / row_limit)
    
    df = dplyr::filter(df, !!dplyr::sym(id_column) %% mod_value == 0)
  }
  
  # output
  output = utils::head(df, row_limit)
  return(dplyr::collect(output))
}
  
## Explore dataset with report -------------------------------------------- ----
#' Explore dataset with report
#' 
#' Provide a wrapper for the explore::report to simplify use - especially for
#' large tables and remote datasets.
#'
#' @param df a data.frame (local or remote)
#' @param id_column (optional) the column of a numeric id to be used for
#' filtering (as per `filter_to_limited_number_of_rows`).
#' @param target (optional) a column to split the exploration by as per
#' `explore::report`.
#' @param output_file (optional) name for the report file - will have the
#' date-time as a suffix. File extension .html is added automatically.
#' @param output_dir (optional) the folder to save the report. Defaults to
#' current working directory. Errors if you do not have write permission to
#' this location.
#' 
#' @return the file name of the report just created.
#' 
#' @export
#' 
explore_report = function(
    df,
    id_column = NA,
    target = NA,
    output_file = "df explored",
    output_dir = getwd()
){
  # checks
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.na(id_column) || is.character(id_column))
  stopifnot(is.na(id_column) || id_column %in% colnames(df))
  stopifnot(is.na(target) || is.character(target))
  stopifnot(is.na(target) || target %in% colnames(df))
  stopifnot(is.character(output_file))
  stopifnot(is.character(output_dir))
  stopifnot(is.na(target) || target %in% colnames(df))
  
  # filter
  df = filter_to_limited_number_of_rows(df = df, row_limit = 10000, id_column = id_column)
  
  # add date-time
  clean_time = format(Sys.time(), "%Y-%m-%d %H%M%S")
  output_file = paste(clean_time, output_file)
  
  # explore
  if(is.na(target)){
    sink("tmp")
    on.exit(sink())
    explore::report(data = df, output_file = output_file, output_dir = output_dir)
  } else {
    sink("tmp")
    on.exit(sink())
    explore::report(data = df, target = !!dplyr::sym(target), output_file = output_file, output_dir = output_dir)
  }
  on.exit(unlink("tmp"), add = TRUE)
  return(paste0(output_file,".html"))
}
