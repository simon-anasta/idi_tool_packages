################################################################################
# Description: Confidentialisation checking functionality
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Specific long-thin data format used and output by these functions.
# - Designed to work with in-memory R data.frames.
#
# Issues:
#
################################################################################

## check rounded to specified base (array) -------------------------------- ----
#' Check rounding of array
#' 
#' @param input_array a numeric array to test for rounding.
#' @param base the base for the array to be rounded to. Defaults to 3.
#' @param na.rm T/F, whether missing values should be treated as pass or fail.
#' Defaults to TRUE, so missing values are considered rounded.
#' 
#' @return T/F. whether all the values in the array are rounded to `base`.
#' 
#' @export
check_rounding_to_base_array <- function(input_array, base = 3, na.rm = TRUE){
  stopifnot(is.numeric(base))
  stopifnot(is.logical(na.rm))
  # require input array has length
  stopifnot(length(input_array) > 0)
  # check numeric
  stopifnot(all(is.numeric(input_array) | is.logical(input_array), na.rm = TRUE))
  
  # warn if only NA's
  if(length(input_array) == sum(is.na(input_array))){
    warning("all input values are NA")
  }
  
  result = all(input_array %% base == 0, na.rm = na.rm)
  
  return(ifelse(is.na(result), FALSE, result))
}

## check rounded to specified base (data.frame) --------------------------- ----
#' Check rounding of column in data.frame
#' 
#' @param df the data.frame to check rounding of.
#' @param column the name of the column of `df` to be checked.
#' @param base the base for the array to be rounded to. Defaults to 3.
#' @param na.rm T/F, whether missing values should be treated as pass or fail.
#' Defaults to TRUE, so missing values are considered rounded.
#' 
#' @return T/F. whether all the values in the array are rounded to `base`.
#' 
#' @export
check_rounding_to_base_df <- function(df, column, base = 3, na.rm = TRUE){
  # df is a data.frame
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  # df is a local dataset (not remote)
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  # column is part of df
  stopifnot(is.character(column))
  stopifnot(column %in% colnames(df))
  
  # run checks
  col_to_array = df[[column]]
  
  return(check_rounding_to_base_array(col_to_array, base = base, na.rm = na.rm))
}

## check graduated rounding (data.frame) ---------------------------------- ----
#' Check graduated rounding of column in data.frame
#' 
#' @param df the data.frame to check rounding of.
#' @param column the name of the column of `df` to be checked.
#' @param na.rm T/F, whether missing values should be treated as pass or fail.
#' Defaults to TRUE, so missing values are considered rounded.
#' 
#' @return T/F. whether all the values in the array are rounded.
#' 
#' @details
#' Graduated rounding requires values of different magnitudes are rounded
#' to different based. This is done according to the following:
#' 
#' | values  | base |
#' |---------|------|
#' |    0-18 |   3  |
#' |     19  |   2  |
#' |   20-99 |   5  |
#' | 100-999 |  10  |
#' |   1000+ | 100  |
#' 
#' @export
check_graduated_rounding_df <- function(df, column, na.rm = TRUE){
  # df is a data.frame
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  # df is a local dataset (not remote)
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  # column is part of df
  stopifnot(is.character(column))
  stopifnot(column %in% colnames(df))
  stopifnot(is.logical(na.rm))
  
  # run checks
  col_to_array = df[[column]]
  
  expected_base = dplyr::case_when(
    0 <= abs(col_to_array) & abs(col_to_array) < 19 ~ 3,
    19 <= abs(col_to_array) & abs(col_to_array) < 20 ~ 2,
    20 <= abs(col_to_array) & abs(col_to_array) < 100 ~ 5,
    100 <= abs(col_to_array) & abs(col_to_array) < 1000 ~ 10,
    1000 <= abs(col_to_array) ~ 100
  )
  
  results = col_to_array %% expected_base == 0
  
  if(sum(is.na(results)) == length(results)){
    warning("all input values are NA")
  }
  if(na.rm){
    results = results | is.na(col_to_array)
  }
  
  result = all(results, na.rm = na.rm)
  return(ifelse(is.na(result), FALSE, result))
}

## check random rounding -------------------------------------------------- ----
#' Check randomness of rounding in data.frame
#' 
#' @param df the data.frame to check rounding of.
#' @param raw_col the name of the raw/unrounded column of `df`.
#' @param conf_col the name of the confidentialised/rounded column of `df`.
#' @param base the base for the array to be rounded to. Defaults to 3.
#' @param print_ratios T/F, whether to display the ratio between the different
#' rounding differences.
#' 
#' @export
check_random_rounding <- function(df, raw_col, conf_col, base = 3, print_ratios = FALSE){
  # df is a local data.frame
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  # columns are part of df
  stopifnot(is.character(raw_col))
  stopifnot(raw_col %in% colnames(df))
  stopifnot(is.character(conf_col))
  stopifnot(conf_col %in% colnames(df))
  
  # check rounding
  rounded_to_base = check_rounding_to_base_df(df, conf_col, base = base)
  
  rounding_diff = df[[conf_col]] - df[[raw_col]]
  rounding_diff = rounding_diff[!is.na(rounding_diff)]
  rounding_diff = rounding_diff[rounding_diff != 0]
  
  # warn if difference is too large for rounding
  if(any(abs(rounding_diff) >= base)){
    warning("rounding not random - difference exceeds the base")
  }
  # warn if all rounding up
  if(all(rounding_diff >= 0)){
    warning("rounding not random - all values rounded up")
  }
  # warn if all rounding down
  if(all(rounding_diff <= 0)){
    warning("rounding not random - all values rounded down")
  }
  
  # table of ratios of each difference amount
  
  diff_df = data.frame(round_by = 1:(base - 1))
  diff_df$count = sapply(
    diff_df$round_by,
    function(x){ sum(abs(rounding_diff) == x) }
  )
  diff_df$actual_percent = diff_df$count / length(rounding_diff)
  diff_df$expect_percent = (base - diff_df$round_by) / ((base - 1) * base / 2)
  diff_df$ratio = diff_df$actual_percent / diff_df$expect_percent
  
  # warn if distribution of rounding is not expected pattern
  # allows a 20% variation
  # ignores cases where fewer than 5 observations
  if(!all(diff_df$count < 5 | (1 - 0.2 <= diff_df$ratio & diff_df$ratio <= 1 + 0.2))){
    warning("rounding not random - actual proportions differ from expected by too much")
  }
  if(print_ratios){
    print(diff_df)
  }
  
  return(rounded_to_base)
}

## check suppression of small counts -------------------------------------- ----
#' Check small count suppression
#' 
#' @param df the data.frame to check for suppression.
#' @param suppressed_col the name of the column to check for suppression.
#' @param threshold the minimum acceptable count. Values less than (but not
#' equal to) the threshold should be suppressed.
#' @param count_col the name of the column to compare with the threshold.
#' Optional, defaults to `suppress_col` if not provided. The intention is that
#' `count_col` contains the counts of unit records.
#' 
#' @return T/F, whether all records in `suppress_col` are suppressed (replaced
#' with missing) where the count is below the threshold.
#' 
#' @export
check_small_count_suppression <- function(df, suppressed_col, threshold, count_col = suppressed_col){
  # df is a local data.frame
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  # columns are part of df
  stopifnot(is.character(suppressed_col))
  stopifnot(suppressed_col %in% colnames(df))
  stopifnot(is.character(count_col))
  stopifnot(count_col %in% colnames(df))
  # numeric threshold
  stopifnot(is.numeric(threshold))
  
  # check suppression
  suppression_required = df[[count_col]] < threshold
  suppression_required = ifelse(is.na(suppression_required), TRUE, suppression_required)
  suppressed_vals = df[[suppressed_col]][suppression_required]
  return(all(is.na(suppressed_vals)))
}

## check for absence of zero counts --------------------------------------- ----
#' Check for the absence of zero counts
#' 
#' If small non-zero counts are suppressed and zero counts do not appear in the
#' dataset, then there is a confidentiality risk that true zeros could be
#' recovered because they are handled differently from small non-zero counts.
#' 
#' @param df the data.frame to check in long-thin format.
#' @param conf_count_col the name of the column of counts to check.
#' @param print_on_fail T/F, whether to print at least one combination that
#' is missing from the data.frame on failure.
#' 
#' @return T/F, whether there is no detected risk of zero counts being treated
#' differently from small, suprpessed counts.
#' 
#' @details
#' Returns TRUE if the absence of zero counts does not pose a confidentiality
#' risk - either there are no suppressed counts OR all combinations of
#' labels/groups appear in the data.frame. Hence true zero values can not be
#' infered by the absence of a label/group from the data.frame.
#' 
#' As a summarised dataset may be produced by appending multiple summaries
#' we run the analysis within each combination.
#' 
#' If print_on_fail = TRUE then at least one combination that is absent from
#' the dataset is printed.
#' 
#' If a risk is identified (function returns FALSE), there are
#' two common solutions:
#' (1) remove all rows that contain suppression of small non-zero counts
#' (2) use expand_to_include_zero_counts to add zero count rows into data.frame
#' 
check_absence_of_zero_counts <- function(df, conf_count_col, print_on_fail = FALSE){
  # df is a local data.frame in required format
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  stopifnot(has_long_thin_format(df))
  # columns are part of df
  stopifnot(is.character(conf_count_col))
  stopifnot(conf_count_col %in% colnames(df))
  stopifnot(is.logical(print_on_fail))
  
  column_names = colnames(df)
  # column groups
  col00 = column_names[grepl("^col[0-9][0-9]$", column_names)]
  val00 = column_names[grepl("^val[0-9][0-9]$", column_names)]
  summary_var = "summarised_var"
  
  # convert all factor columns to character
  df = dplyr::mutate(df, dplyr::across(dplyr::where(is.factor), as.character))
  
  # all sub-summaries with the df
  subsummaries = dplyr::select(df, dplyr::all_of(c(col00, summary_var)))
  subsummaries = dplyr::distinct(subsummaries)
  
  # iterate through all sub-summaries
  for(ii in 1:nrow(subsummaries)){
    # create sub-summary dataset
    this_subsummary = dplyr::semi_join(df, subsummaries[ii,], by = c(col00, summary_var))
    
    # if no NA values then there are no concerns - go to next sub-summary
    if(!any(is.na(this_subsummary[[conf_count_col]]))){ next }
    
    # create comparison with all rows
    all_row_df = tidyr::expand(this_subsummary, !!!rlang::syms(c(col00,val00, summary_var)))
    # combinations missing from current sub-summary
    missing_rows = dplyr::anti_join(all_row_df, this_subsummary, by = c(col00,val00, summary_var))
    
    # if any values missing
    if(nrow(missing_rows) != 0){
      if(print_on_fail){ print(utils::head(missing_rows)) }
      return(FALSE)
    }
  }
  
  # we have checked all sub-summaries and found no concerns
  return(TRUE)
}

## expand to include zero counts ------------------------------------------ ----
#' Expand data.frame to include missing rows
#' 
#' If small non-zero counts are suppressed and zero counts do not appear in the
#' data.frame, then there is a confidentiality risk that true zeros could be
#' recovered because they are handled differently from small non-zero counts.
#' 
#' Where a risk is identified (by `check_absence_of_zero_counts`), one solutions
#' is to add zero count rows back into the data.frame - the purpose of this
#' function.
#' 
#' @param df the data.frame to modify in long-thin format.
#' 
#' @return a data.frame with all interactions of `col` and `val`.
#' As a summarised dataset may be produced by appending multiple summaries
#' we expand within each combination.
#' 
#' @export
expand_to_include_zero_counts <- function(df){
  # df is a local data.frame in required format
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  stopifnot(has_long_thin_format(df))
  
  column_names = colnames(df)
  # column groups
  col00 = column_names[grepl("^col[0-9][0-9]$", column_names)]
  val00 = column_names[grepl("^val[0-9][0-9]$", column_names)]
  summary_var = "summarised_var"
  
  # convert all factor columns to character
  df = dplyr::mutate(df, dplyr::across(dplyr::where(is.factor), as.character))
  
  # all sub-summaries with the df
  subsummaries = dplyr::select(df, dplyr::all_of(c(col00, summary_var)))
  subsummaries = dplyr::distinct(subsummaries)
  
  expanded_rows_list = lapply(
    1:nrow(subsummaries),
    function(ii){
      # create sub-summary dataset
      this_subsummary = dplyr::semi_join(df, subsummaries[ii,], by = c(col00, summary_var))
      
      # create comparison with all rows
      all_row_df = tidyr::expand(this_subsummary, !!!rlang::syms(c(col00,val00, summary_var)))
      # sub-summary with all combinations included
      expanded_rows = dplyr::right_join(this_subsummary, all_row_df, by = c(col00,val00, summary_var))
    }
  )
  
  output = dplyr::bind_rows(expanded_rows_list)
  output = dplyr::select(output, dplyr::all_of(column_names))
  return(output)
}

## check long-thin results ------------------------------------------------ ----
#' Check most common conditions for long-thin output
#' 
#' Runs and reports on random rounding, suppression, and handling of zeros
#' using the most common defaults. Designed to take the direct output of 
#' `summarise_and_label_over_lists` followed by `confidentialise_results`.
#' 
#' @param df the data.frame to check in long-thin format.
#' @param BASE the base for random rounding. Defaults to 3.
#' @param COUNT_THRESHOLD the minimum acceptable count. Where `count` or
#' `distinct` are below this threshold they should be suppressed. Defaults
#' to 6.
#' @param SUM_THRESHOLD the minimum accepted count to output a sum. If either
#' `count` or `distinct` are below this threshold, the sum column should
#' be suppressed. Defaults to 20.
#' 
#' @return a message summarising all the checks that were run with pass,
#' fail, and skip messages. Skip means a check was not applicable as the
#' required columns were not present in `df`. Pass means confidentiality
#' rules upheld, fail means rules was not upheld.
#' 
#' @export
check_confidentialised_results <- function(
    df,
    BASE = 3,
    COUNT_THRESHOLD = 6,
    SUM_THRESHOLD = 20
){
  # df is a local data.frame in required format
  stopifnot(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df))
  df_classes = tolower(class(df))
  stopifnot(!any(grepl("sql", df_classes, ignore.case = TRUE)))
  stopifnot(has_long_thin_format(df))
  stopifnot(is.numeric(BASE))
  stopifnot(is.numeric(COUNT_THRESHOLD))
  stopifnot(is.numeric(SUM_THRESHOLD))
  
  # log for output
  log = list(column = c(), check = c(), result = c())
  # record log message
  record_log = function(log, column, check, result){
    log$column = c(log$column, column)
    log$check = c(log$check, check)
    log$result = c(log$result, result)
    return(log)
  }
  
  #### distinct ----------------------------------------
  col = "conf_distinct"
  
  chk = glue::glue("checked for RR{BASE}")
  result = tryCatch(
    ifelse(check_random_rounding(df, "raw_distinct", "conf_distinct", BASE), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)
  
  chk = glue::glue("suppressed if raw < {COUNT_THRESHOLD}")
  result = tryCatch(
    ifelse(check_small_count_suppression(df, "conf_distinct", COUNT_THRESHOLD, "raw_distinct"), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)
  
  #### count ----------------------------------------
  col = "conf_count"
  
  chk = glue::glue("checked for RR{BASE}")
  result = tryCatch(
    ifelse(check_random_rounding(df, "raw_count", "conf_count", BASE), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)
  
  chk = glue::glue("suppressed if raw < {COUNT_THRESHOLD}")
  result = tryCatch(
    ifelse(check_small_count_suppression(df, "conf_count", COUNT_THRESHOLD, "raw_count"), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)
  
  #### sum ----------------------------------------
  col = "conf_sum"
  
  chk = glue::glue("suppressed if raw < {SUM_THRESHOLD}")
  r1 = tryCatch(
    ifelse(check_small_count_suppression(df, "conf_sum", SUM_THRESHOLD, "raw_distinct"), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  r2 = tryCatch(
    ifelse(check_small_count_suppression(df, "conf_sum", SUM_THRESHOLD, "raw_count"), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  
  result = dplyr::case_when(
    r1 == "fail" | r2 == "fail" ~ "fail",
    r1 == "skip" & r2 == "skip" ~ "skip",
    r1 == "pass" | r2 == "pass" ~ "pass"
  )
  
  log = record_log(log, col, chk, result)
  
  #### zero counts ----------------------------------------
  
  result = tryCatch(
    ifelse(check_absence_of_zero_counts(df, "conf_count", print_on_fail = FALSE), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, "all", "absence of zero counts", result)
  
  # convert log to formatted message - ensure alignment of checks
  format_string = glue::glue("%{max(nchar(log$column))}s %{max(nchar(log$check))}s : %s")
  msg = sprintf(format_string, log$column, log$check, toupper(log$result))
  
  return(msg)
}
