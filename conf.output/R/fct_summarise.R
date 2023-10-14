################################################################################
# Description: Summarisation functionality
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Specific long-thin data format used and output by these functions.
# - Designed to work with both local/in-memory R data.frames and remote/SQL/
#   dbplyr-translated data sources.
#
# Issues:
#
################################################################################

## convert lists to iterator ---------------------------------------------- ----
#' Cross-product of column names
#' 
#' Creates the cross-products of its inputs. Designed for receiving column names
#' and creating combinations to group-by in analysis. For example if we want
#' every pairwise combination of demographic columns then store the names of
#' the demographic columns in an array called `demographic_columns` and call:
#' `cross_product_column_names(demographic_columns, demographic_columns)`
#' 
#' @param ... any number of arrays, to take the cross-product between.
#' @param always an array of values that should always be included in the output
#' combination.
#' @param drop.dupes.within T/F, controls whether duplicated values within each
#' output set are discarded. For example: `(a,b,c,a,b)` becomes `(a,b,c)`.
#' Note: dplyr::group_by requires no duplicates so turning this off
#' may produce errors if the output is used for summarizing results.
#' Defaults to TRUE.
#' @param drop.dupes.across T/F, control whether duplicated sets of values
#' across the output are discarded. For example: `list(c(a,b,c), c(a,c,b))`
#' will only output `c(a,b,c)`.
#' Defaults to TRUE.
#' 
#' @return A list of arrays produced by taking the cross-products of each input
#' group, appending the always inputs, and tidying by removing duplication
#' where applicable.
#' 
#' @export
#' 
#' @examples
#' # setup
#' grp1 = c("d","e")
#' grp2 = c("f","g")
#' grp3 = c("a","b","c")
#' # output
#' cross_product_column_names(grp1, grp2, always = grp3)
#' cross_product_column_names(grp3, grp3)
#' cross_product_column_names(grp3, grp3, drop.dupes.across = FALSE)
#' cross_product_column_names(grp3, grp3, drop.dupes.within = FALSE)
#' 
cross_product_column_names = function(
    ...,
    always = NULL,
    drop.dupes.within = TRUE,
    drop.dupes.across = TRUE
){
  stopifnot(is.null(always) | is.character(always))
  stopifnot(is.logical(drop.dupes.within))
  stopifnot(is.logical(drop.dupes.across))
  # setup
  groups = list(...)
  output = list(always)
  
  # iterate through all combinations
  for(group in groups){
    new_list = list()
    for(grp_element in group){
      for(output_component in output){
        new_list = c(new_list, list(c(output_component, grp_element)))
      }
    }
    output = new_list
  }
  
  # drop duplicates
  if(drop.dupes.within){
    output = lapply(output, unique)
  }
  
  # remove duplicates across cross-products (ignores order)
  if(drop.dupes.across){
    output = lapply(output, sort)
    output = unique(output)
  }
  
  return(output)
}

## check if long-thin format used ----------------------------------------- ----
#' Check long-thin format used
#' 
#' For consistency between summarise, confidentialise, and check functionality
#' we impose a standard format for summarised output. Termed 'long-thin format'.
#' 
#' This format has pairs of 'col' and 'val' columns the describe the group
#' represented by the data row. Followed by columns for outputs.
#'
#' @param df a data.frame to check for long-thin format
#' 
#' @return T/F, whether the data.frame complies with our long-thin format.
#' 
#' @details
#' Long-thin format takes the form:
#' `col01, val01, ..., col99, val99, summarised_var, distinct, count,  sum`
#' Where the last three columns are optional, and can have a `raw_` or `conf_`
#' prefix.
#' 
#' Hence we conduct the following checks to confirm a data frame is in this
#' format: (1) every column name is col_, val_, distinct, count, or sum, and
#' (2) every col* has a matching val*
#' 
#' @export
#' 
has_long_thin_format <- function(df){
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  
  column_names = colnames(df)
  
  col00 = grepl("^col[0-9][0-9]$", column_names)
  val00 = grepl("^val[0-9][0-9]$", column_names)
  summary_var = grepl("^summarised_var$", column_names)
  summary = grepl("^(raw_|conf_|)(count|sum|distinct)$", column_names)
  
  # same number of col and val
  same_number = sum(col00, na.rm = TRUE) == sum(val00, na.rm = TRUE)
  expected_columns = all(col00 | val00 | summary_var | summary)
  
  return(same_number & expected_columns)
}

## single summarise and label --------------------------------------------- ----
#' Single summarise and label
#'
#' Produces a summary of the specified groups with distinct, count, or sum.
#' Does so in a way that is robust to whether df is a local or a remote table.
#' Returns a local table in long-thin format.
#' 
#' Used on its own, produces output consistent with `dplyr::group_by` followed
#' by `dplyr::summarise`. It's advantage is its consistent output format, and
#' how it is use with `summarise_and_label_over_lists` to produce many
#' summaries with a single command.
#' 
#' @param df a data.frame to summarise. Can be in-memory or remote accessed
#' with dbplyr.
#' @param group_by_cols an array of column names to group the table by.
#' @param summarise_col the name of a column to summarise.
#' @param make_distinct T/F, whether the output should include a count of
#' distinct values from `summarise_col`.
#' @param make_count T/F, whether the output should include a count of values
#' from `summarise_col`.
#' @param make_sum T/F, whether the output should include the sum of values
#' from `summarise_col`.
#' @param clean hat cleaning, if any, to apply to the summarised column. Must
#' be one of `{"none", "na.as.zero", "zero.as.na"}`. Defaults to `"none"`. See
#' details for explanation.
#' @param remove.na.from.groups T/F, whether missing values in the data should
#' be excluded when grouping for summarisation. For example, if grouping by age
#' should the output include a row for the records with no age.
#' Defaults to TRUE.
#' @param query_path If provided and data source is SQL will attempt to save a
#' copy of the SQL code sent to/executed on the database to the provided folder.
#' Save occurs before execution, hence useful for debugging.
#'   
#' @return a local data.frame in long-thin format containing the summarised
#' results in long-thin format.
#' 
#' @details
#' Argument `clean` determines how missing values are handled. Missing values
#' are excluded from the results - they are neither counted nor summed.
#' 
#' The default option `"none"` leaves missing values as they appear in the
#' data. The alternatives `"na.as.zero"` and `"zero.as.na"` convert between
#' missing values and zero.
#' 
#' One example where this control is useful is producing average income: Should
#' it be an average over all people or an average over those people with income?
#' (As means have additional confidentiality rules, this function produces the
#' sum/total and the count separately so that confidentiality can be applied
#' to the numerator and denominator separator if required.)
#' 
#' @export
summarise_and_label <- function(
    df,
    group_by_cols,
    summarise_col,
    make_distinct,
    make_count,
    make_sum,
    clean = "none", # {"none", "na.as.zero", "zero.as.na"}
    remove.na.from.groups = TRUE,
    query_path = NA
){
  #### checks ----
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.character(group_by_cols))
  stopifnot(all(group_by_cols %in% colnames(df)))
  stopifnot(is.character(summarise_col))
  stopifnot(length(summarise_col) == 1)
  stopifnot(summarise_col %in% colnames(df))
  stopifnot(clean %in% c("none", "na.as.zero", "zero.as.na"))
  stopifnot(is.logical(make_distinct))
  stopifnot(is.logical(make_count))
  stopifnot(is.logical(make_sum))
  stopifnot(make_distinct | make_count | make_sum)
  
  #### cleaning ----
  df = dplyr::select(df, all_of(c(group_by_cols, summarise_col)))
  
  if(remove.na.from.groups){
    # apply filters in a single step
    tmp = paste0(" !is.na(", group_by_cols, ") ")
    df = dplyr::filter(df, `!!!`(rlang::parse_exprs(tmp)))
  }
  
  if(clean == "na.as.zero"){
    df[[summarise_col]] = ifelse(is.na(df[[summarise_col]]), 0, df[[summarise_col]])
  } else if(clean == "zero.as.na"){
    df[[summarise_col]] = ifelse(df[[summarise_col]] == 0, NA, df[[summarise_col]])
  }
  df = dplyr::filter(df, !is.na(!!rlang::sym(summarise_col)))
  
  #### summarise clauses ----
  summary_clauses = list()
  
  if(make_distinct){
    clause = glue::glue("dplyr::n_distinct({summarise_col})")
    clause = rlang::parse_expr(clause)
    summary_clauses = c(summary_clauses, list(distinct = clause))
  }
  
  if(make_count){
    clause = glue::glue("dplyr::n()")
    clause = rlang::parse_expr(clause)
    summary_clauses = c(summary_clauses, list(count = clause))
  }
  
  if(make_sum){
    clause = glue::glue("sum({summarise_col}, na.rm = TRUE)")
    clause = rlang::parse_expr(clause)
    summary_clauses = c(summary_clauses, list(sum = clause))
  }

  #### summarise ----
  output_df = dplyr::group_by(df, !!!rlang::syms(group_by_cols))
  output_df = dplyr::summarise(output_df, !!!summary_clauses, .groups = "drop")
  
  #### output query if relevant ----
  if(!is.na(query_path) & "tbl_sql" %in% class(output_df)){
    dbplyr.helpers::save_to_sql_script(output_df, "make _summary", query_path = query_path)
  }
  
  ## fetch results ----
  output_df = dplyr::collect(output_df)
  
  #### label ----
  # converting to long-thin format
  
  col_order = c()
  
  for(ii in 1:length(group_by_cols)){
    col = sprintf("col%02d", ii)
    val = sprintf("val%02d", ii)
    
    output_df[[col]] = group_by_cols[ii]
    output_df = dplyr::rename(output_df, !!rlang::sym(val) := !!rlang::sym(group_by_cols[ii]))
    output_df[[val]] = as.character(output_df[[val]])
    
    col_order = c(col_order, col, val)
  }
  
  col_order = c(col_order, "summarised_var")
  if(make_distinct){
    col_order =  c(col_order, "distinct")
  }
  if(make_count){
    col_order = c(col_order, "count")
  }
  if(make_sum){
    col_order = c(col_order, "sum")
  }
  
  output_df = dplyr::mutate(output_df, summarised_var = summarise_col)
  output_df = dplyr::select(output_df, !!!rlang::syms(col_order))
  
  #### conclude ----
  stopifnot(has_long_thin_format(output_df))
  return(output_df)
}

## summarise and label from list ------------------------------------------ ----
#' Summarise results for all combinations in list
#' 
#' Produces a summary of every variable given in `summarise_list` for every
#' group given in `group_by_list`. Does so in a way that is robust to whether
#' df is a local or a remote table. Returns a local table in long-thin format.
#' 
#' Output produced is consistent with running multiple `dplyr::group_by` and
#' `dplyr::summarise` commands. But produces many summaries with a single
#' command and appends their output.
#' 
#' @param group_by_list a list where each entry is an array of column names to
#' group the table by. Each item in `group_by_list` will become an input to
#' `group_by_cols` in `summarise_and_label`.
#' @param summarise_list a list where each entry is the name of a column to
#' summarise. Each item in `summarise_list` will become an input to
#' `summarise_col` in `summarise_and_label`.
#' @inheritParams summarise_and_label
#'
#' @return a local data.frame in long-thin format containing the all the 
#' summarised results appended together in long-thin format.
#' 
#' @inheritSection summarise_and_label details
#' 
#' @export
#' 
summarise_and_label_over_lists <- function(
    df, 
    group_by_list,
    summarise_list,
    make_distinct,
    make_count,
    make_sum,
    clean = "none", # {"none", "na.as.zero", "zero.as.na"}
    remove.na.from.groups = TRUE,
    query_path = NA
){
  #### checks ----
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.list(group_by_list))
  stopifnot(length(group_by_list) >= 1)
  stopifnot(is.list(summarise_list))
  stopifnot(length(summarise_list) >= 1)
  for(ii in 1:length(group_by_list)){
    stopifnot(is.character(group_by_list[[ii]]))
    stopifnot(all(group_by_list[[ii]] %in% colnames(df)))
  }
  for(ii in 1:length(summarise_list)){
    stopifnot(is.character(summarise_list[[ii]]))
    stopifnot(summarise_list[[ii]] %in% colnames(df))
  }
  
  #### make all combinations ----
  output_list = list()
  
  for(gg in group_by_list){
    for(ss in summarise_list){
      this_df = summarise_and_label(
        df,
        gg, 
        ss, 
        make_distinct, 
        make_count, 
        make_sum, 
        clean, 
        remove.na.from.groups,
        query_path
      )
      
      output_list = c(output_list, list(this_df))
    }
  }
  
  #### conclude ----
  
  # ensure all val columns are of type character
  output_list = lapply(
    output_list, 
    function(df){
      dplyr::mutate(df, dplyr::across(dplyr::starts_with("val"), as.character))
    }
  )
  
  # list of df's into a single df
  output_df = dplyr::bind_rows(output_list)
  
  stopifnot(has_long_thin_format(output_df))
  return(output_df)
}

