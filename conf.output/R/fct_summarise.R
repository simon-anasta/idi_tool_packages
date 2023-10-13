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

## Filter to limited number of rows --------------------------------------- ----
#' Filter to a limited number of rows
#' 
#' 


## convert lists to iterator -------------------------------------------
#'
#' Returns a list containing all cross-products of  column names.
#' Intended for grouping by in analysis.
#' 
#' always parameter is included in each output.
#' 
#' drop.dupes.within controls whether duplicated parameters within each set
#' of output columns are discarded.
#' For example: (a,b,c,a,b) becomes (a,b,c).
#' Note: dplyr::group_by requires no duplicates so turning this off
#' may produce errors if the output is used for summarising results.
#' 
#' drop.dupes.across control whether duplicate sets of parameters are discarded.
#' For example: (a,b,c) and (a,c,b) will only output (a,b,c)
#' 
#' For example: always = (a,b,c), grp1 = (d,e), grp2 = (f,g)
#' Returns list containing:
#' (a,b,c,d,f), (a,b,c,d,g), (a,b,c,e,f), (a,b,c,e,g)
#' 
cross_product_column_names <- function(...,
                                       always = NULL,
                                       drop.dupes.within = TRUE,
                                       drop.dupes.across = TRUE){
  # checks
  assert(is.null(always) | is.character(always), "[always] must be character")
  # setup
  groups = list(...)
  output = list(always)
  
  # iterate through all combinations
  for(group in groups){
    new_list = list()
    for(gg in group){
      for(item in output){
        new_list = c(new_list, list(c(item, gg)))
      }
    }
    output = new_list
  }
  
  # drop duplicates
  if(drop.dupes.within){
    new_list = list()
    for(item in output){
      new_list = c(new_list, list(unique(item)))
    }
    output = new_list
  }
  
  # remove duplicates across cross-products (ignores order)
  if(drop.dupes.across){
    new_list = list()
    sort_list = list()
    for(item in output){
      sorted_item = sort(item)
      if(!list(sorted_item) %in% sort_list){
        sort_list = c(sort_list, list(sorted_item))
        new_list = c(new_list, list(item))
      }
    }
    output = new_list
  }
  
  return(output)
}

## check if long-thin format used ---------------------------------------------
#'
#' Checks the our long-thin format and column labels have been used.
#' Formats:
#' col01 val01 ... col99 val99 summarised_var distinct count  sum
#' col01 val01 ... col99 val99 summarised_var raw_distinct raw_count raw_sum conf_distinct conf_count conf_sum
#' 
#' Checks
#' - every column name is col_, val_, distinct, count, or sum
#' - every col_ has a matching val_
#' 
has_long_thin_format <- function(df){
  assert(is.data.frame(df) | dplyr::is.tbl(df), "[df] must be a data.frame")
  
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

## single summarise and label -------------------------------------------------
#'
#' Produces a summary of the specified groups with distinct, count, or sum.
#' Does so in a way that is robust to whether df is a local or a remote table.
#' Returns a local table in long-thin format.
#' 
#' argument clean allows three options:
#' - none = no cleaning
#' - na.as.zero = replaces NA values with zero before summarising
#' - zero.as.na = replaces zero values with NA before summarising
#' NA values are excluded from distinct, count, and sum.
#' 
#' argument remove.na.from.groups determines whether missing values are
#' removed or kept in the grouping columns.
#'
summarise_and_label <- function(df,
                                group_by_cols,
                                summarise_col,
                                make_distinct,
                                make_count,
                                make_sum,
                                clean = "none", # {"none", "na.as.zero", "zero.as.na"}
                                remove.na.from.groups = TRUE){
  #### checks ----
  assert(is.data.frame(df) | dplyr::is.tbl(df), "[df] must be a data.frame")
  assert(is.character(group_by_cols), "[group_by_cols] must be of type character")
  assert(all(group_by_cols %in% colnames(df)), "at least one group column is not a column of [df]")
  assert(is.character(summarise_col), "[summarise_col] must be of type character")
  assert(length(summarise_col) == 1, "only one [summarise_col] can be specified")
  assert(summarise_col %in% colnames(df), "[summarise_col] must be a column of [df]")
  assert(clean %in% c("none", "na.as.zero", "zero.as.na"), "[clean] accepts only three options see documentation")
  assert(is.logical(make_distinct), "[make_distinct] must be type logical")
  assert(is.logical(make_count), "[make_count] must be type logical")
  assert(is.logical(make_sum), "[make_sum] must be type logical")
  
  #### cleaning ----
  df = dplyr::select(df, all_of(c(group_by_cols, summarise_col)))
  
  if(remove.na.from.groups){
    # apply filters in a single step
    tmp = paste0(" !is.na(", group_by_cols, ") ")
    df = dplyr::filter(df, `!!!`(rlang::parse_exprs(tmp)))
    
    ## prev version
    # for(gg in group_by_cols){
    #   df = dplyr::filter(df, !is.na(!!sym(gg)))
    # }
  }
  
  if(clean == "na.as.zero"){
    df = dplyr::mutate(df, !!sym(summarise_col) := ifelse(is.na(!!sym(summarise_col)), 0, !!sym(summarise_col)))
  } else if(clean == "zero.as.na"){
    df = dplyr::mutate(df, !!sym(summarise_col) := ifelse(!!sym(summarise_col) == 0, NA, !!sym(summarise_col)))
  }
  
  #### summarise ----
  output_df = df %>%
    dplyr::select(!!!syms(group_by_cols)) %>%
    dplyr::distinct() %>%
    dplyr::collect()
  
  df = dplyr::group_by(df, !!!syms(group_by_cols))
  
  if(make_distinct){
    tmp_df = df %>%
      dplyr::filter(!is.na(!!sym(summarise_col))) %>%
      dplyr::summarise(distinct = dplyr::n_distinct(!!sym(summarise_col)), .groups = "drop")
    
    # output query if relevant
    if("tbl_sql" %in% class(tmp_df)){
      save_to_sql(tmp_df %>% dbplyr::sql_render() %>% as.character(), "make distinct")
    }
    
    output_df = tmp_df %>%
      dplyr::collect() %>%
      dplyr::right_join(output_df, by = group_by_cols)
  }
  
  if(make_count){
    tmp_df = df %>%
      dplyr::summarise(count = sum(ifelse(is.na(!!sym(summarise_col)), 0, 1), na.rm = TRUE), .groups = "drop")
    
    # output query if relevant
    if("tbl_sql" %in% class(tmp_df)){
      save_to_sql(tmp_df %>% dbplyr::sql_render() %>% as.character(), "make count")
    }
    
    output_df = tmp_df %>%
      dplyr::collect() %>%
      dplyr::right_join(output_df, by = group_by_cols)
  }
  
  if(make_sum){
    tmp_df = df %>%
      dplyr::summarise(sum = sum(!!sym(summarise_col), na.rm = TRUE), .groups = "drop")
    
    # output query if relevant
    if("tbl_sql" %in% class(tmp_df)){
      save_to_sql(tmp_df %>% dbplyr::sql_render() %>% as.character(), "make sum")
    }
    
    output_df = tmp_df %>%
      dplyr::collect() %>%
      dplyr::right_join(output_df, by = group_by_cols)
  }
  
  #### label ----
  # converting to long-thin format
  
  col_order = c()
  
  for(ii in 1:length(group_by_cols)){
    col = sprintf("col%02d", ii)
    val = sprintf("val%02d", ii)
    
    output_df = output_df %>%
      dplyr::mutate(!!sym(col) := group_by_cols[ii]) %>%
      dplyr::rename(!!sym(val) := !!sym(group_by_cols[ii])) %>%
      dplyr::mutate(!!sym(val) := as.character(!!sym(val)))
    
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
  
  output_df = output_df %>%
    dplyr::mutate(summarised_var = summarise_col) %>%
    dplyr::select(!!!syms(col_order))
  
  #### conclude ----
  assert(has_long_thin_format(output_df), "output not long-thin formatted as expected")
  return(output_df)
}

## summarise and label from list ----------------------------------------------
#'
#' Produces a summary of every variable given in summarise_list for every
#' group given in group_by_list. Allows for distinct, count, or sum summary.
#' 
#' Does so in a way that is robust to whether df is a local or a remote table.
#' Returns a local table in long-thin format.
#' 
#' argument clean allows three options:
#' - none = no cleaning
#' - na.as.zero = replaces NA values with zero before summarising
#' - zero.as.na = replaces zero values with NA before summarising
#' NA values are excluded from distinct, count, and sum.
#' 
#' argument remove.na.from.groups determines whether missing values are
#' removed or kept in the grouping columns.
#'
summarise_and_label_over_lists <- function(df, 
                                           group_by_list,
                                           summarise_list,
                                           make_distinct,
                                           make_count,
                                           make_sum,
                                           clean = "none", # {"none", "na.as.zero", "zero.as.na"}
                                           remove.na.from.groups = TRUE){
  #### checks ----
  assert(is.data.frame(df) | dplyr::is.tbl(df), "[df] must be of type data.frame")
  assert(is.list(group_by_list), "[group_by_list] must be a list of groups")
  assert(length(group_by_list) >= 1, "at least one group must be provided in [group_by_list]")
  assert(is.list(summarise_list), "[summarise_list] must be a list of groups")
  assert(length(summarise_list) >= 1, "at least one group must be provided in [summarise_list]")
  for(ii in 1:length(group_by_list)){
    assert(is.character(group_by_list[[ii]]), glue::glue("group {ii} is not of type character"))
    assert(all(group_by_list[[ii]] %in% colnames(df)), glue::glue("group {ii} requires columns not in [df]"))
  }
  for(ii in 1:length(summarise_list)){
    assert(is.character(summarise_list[[ii]]), glue::glue("summary column {ii} is not of type character"))
    assert(summarise_list[[ii]] %in% colnames(df), glue::glue("summary column {ii} is not found in [df]"))
  }
  
  #### make all combinations ----
  output_list = list()
  
  for(gg in group_by_list){
    for(ss in summarise_list){
      this_df = summarise_and_label(df, gg, ss, make_distinct, make_count, make_sum, clean, remove.na.from.groups)
      
      output_list = c(output_list, list(this_df))
    }
  }
  
  #### conclude ----
  
  # ensure all val columns are of type character
  output_list = lapply(output_list, function(df){mutate(df, across(starts_with("val"), as.character))})
  
  # list of df's into a single df
  output_df = dplyr::bind_rows(output_list)
  
  assert(has_long_thin_format(output_df), "output not long-thin formatted as expected")
  return(output_df)
}

