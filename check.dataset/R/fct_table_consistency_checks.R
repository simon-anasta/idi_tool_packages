################################################################################
# Description: Table condition checks and asserts
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Many of the functions come in two variants:
#     check_* variants return TRUE/FALSE
#     assert_* variants error on a FALSE, and are silent on TRUE
# - Replicates some existing R functions, but these versions will also work on
#   remote tables as well as local tables.
#
# Issues:
#
################################################################################

## Evaluate numeric comparison from string -------------------------------- ----
#' Evaluate numeric comparison
#'
#' Provides evaluation of comparison when operator is given as text.
#' E.g. evaluation_direction(14, "<", 10) returns 14 < 10 which is FALSE.
#'
#' @param val1 numeric value for left-side of comparison.
#' @param direction mathematical operator for comparison.
#' @param val2 numeric value for right-side of comparison.
#' 
#' @return T/F whether the comparison holds true.
#' 
evaluation_comparison = function(val1, direction, val2) {
  stopifnot(is.numeric(val1))
  stopifnot(is.numeric(val2))
  stopifnot(direction %in% c("==", "<", ">", "<=", ">=", "!="))
  
  answer = eval(parse(text = paste(val1, direction, val2)))
  return(answer)
}

## Number of rows --------------------------------------------------------- ----
#' Number of rows (remote)
#'
#' Provides the same functionality as `nrow` but also works on remote tables.
#'
#' @param df a data.frame (local or remote).
#' 
#' @return The number of rows in the data.frame. Equivalent to `nrow` for 
#' data.frames in local R memory. Also works for tables in remote databases.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' num_row(iris)
#' 
num_row = function(df) {
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  
  answer = dplyr::ungroup(df)
  answer = dplyr::summarise(answer, num = dplyr::n())
  answer = dplyr::collect(answer)
  answer = dplyr::pull(answer, "num")
  
  return(answer)
}

## Number unique values in column(s) -------------------------------------- ----
#' Number of distinct values
#'
#' Counts the number of unique values in a column (or set of columns).
#' Designed to work with remote and local tables.
#'
#' @param df a data.frame (local or remote)
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' 
#' @return The number of distinct values in the columns of `df` given by
#' `col_names`.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' num_unique_entries(iris, "Species")
#' 
num_unique_entries = function(df, col_name) {
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.character(col_name))
  stopifnot(all(col_name %in% colnames(df)))
  
  answer = dplyr::ungroup(df)
  answer = dplyr::select(answer, !!!dplyr::syms(col_name))
  answer = dplyr::distinct(answer)
  answer = dplyr::summarise(answer, num_u = dplyr::n())
  answer = dplyr::collect(answer)
  answer = dplyr::pull(answer, "num_u")
  
  return(answer)
}

## Number missing values in column ---------------------------------------- ----
#' Number of missing values
#' 
#' Counts the number of missing values in a column. If given multiple columns,
#' sums the missing values across columns.
#' 
#' Designed to work with remote and local tables.
#'
#' @param df a data.frame (local or remote)
#' @param col_name the name(s) of columns to check for missing values.
#' Errors if any `col_name` is not a column name of `df`.
#' 
#' @return the total number of missing values in the columns of `df` given by
#' `col_names`.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' num_missing_entries(iris, "Species")
#' num_missing_entries(iris, c("Sepal.Length", "Sepal.Width"))
#' 
num_missing_entries = function(df, col_name) {
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.character(col_name))
  stopifnot(all(col_name %in% colnames(df)))
  
  df = dplyr::ungroup(df)
  
  answer = 0
  
  for (col in col_name) {
    this_answer = dplyr::mutate(df, missing_tmp = ifelse(is.na(!!dplyr::sym(col)), 1, 0))
    this_answer = dplyr::summarise(this_answer, num_na = sum(.data$missing_tmp))
    this_answer = dplyr::collect(this_answer)
    this_answer = dplyr::pull(this_answer, "num_na")
    
    answer = answer + this_answer
  }
  
  return(answer)
}

## Check table size ------------------------------------------------------- ----
#' Check table size
#' 
#' Returns whether number of rows in table is in direction relative to size.
#'
#' @param df a data.frame (local or remote)
#' @param direction mathematical operator for comparison.
#' @param size numeric value to compare against size.
#' 
#' @return T/F for whether the rows of `df` are `direction` of `size`.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' check_size(iris, "==", 10)
#' # FALSE - iris has 150 rows
#' 
#' 
check_size = function(df, direction, size) {
  nn = num_row(df)
  return(evaluation_comparison(nn, direction, size))
}

## Assert table size ------------------------------------------------------ ----
#' Assert table size
#'
#' Asserts number of rows in table is relative to size.
#'
#' @param df a data.frame (local or remote)
#' @param direction mathematical operator for comparison.
#' @param size numeric value to compare against size.
#' 
#' @return Errors if assert fails. Returns number of rows invisibly if passed.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' assert_size(iris, ">=", 100)
#' # no error - iris has 150 rows
#' 
assert_size = function(df, direction, size) {
  num_rows = num_row(df)
  
  if(!evaluation_comparison(num_rows, direction, size)){
    msg = glue::glue("num rows of df = {num_rows} is not {direction} {size}")
    stop(msg)
  }
  
  return(invisible(num_rows))
}

## Check comparison of table sizes ---------------------------------------- ----
#' Check comparison of table sizes
#'
#' Returns T/F for whether direction applies to number of rows in both tables.
#'
#' @param df1 a data.frame (local or remote) for left-side of comparison.
#' @param direction mathematical operator for comparison.
#' @param df2 a data.frame (local or remote) for right-side of comparison.
#' 
#' @return T/F whether the size of the two tables obeys the direction.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' check_size_comparison(head(iris, 10), ">", head(iris, 5))
#' 
check_size_comparison = function(df1, direction, df2) {
  n1 = num_row(df1)
  n2 = num_row(df2)
  return(evaluation_comparison(n1, direction, n2))
}

## Assert comparison of table sizes --------------------------------------- ----
#' Assert comparison of table sizes
#' 
#' Asserts direction applies to number of rows in both tables.
#' 
#' @param df1 a data.frame (local or remote) for left-side of comparison.
#' @param direction mathematical operator for comparison.
#' @param df2 a data.frame (local or remote) for right-side of comparison.
#' 
#' @return Errors if assert fails. Returns number of rows invisibly if passed.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' assert_size_comparison(head(iris), "<", iris)
#' 
assert_size_comparison = function(df1, direction, df2) {
  n1 = num_row(df1)
  n2 = num_row(df2)
  
  if(!evaluation_comparison(n1, direction, n2)){
    msg = glue::glue("num rows of df1 = {n1} is not {direction} num rows of df2 = {n2}")
    stop(msg)
  }
  
  return(invisible(c(n1, n2)))
}

## Check number of unique values ------------------------------------------ ----
#' Check number of distinct values
#'
#' Returns T/F for whether number of distinct values in col_name
#' (or a combination of col_names) `df` is `direction` relative to `size`.
#'
#' @param df a data.frame (local or remote).
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' @param direction mathematical operator for comparison.
#' @param size numeric value to compare against size.
#' 
#' @return T/F whether the number of distinct values in the specified column(s)
#' is consistent with the comparison.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' check_num_distinct(iris, "Species", "!=", 1)
#' check_num_distinct(iris, c("Sepal.Length", "Sepal.Width"), ">", 20)
#' 
check_num_distinct = function(df, col_name, direction, size) {
  uu = num_unique_entries(df, col_name)
  return(evaluation_comparison(uu, direction, size))
}

## Assert number of unique values ----------------------------------------- ----
#' Assert number of distinct values
#' 
#' Assert number of unique values in col_name (or a combination of col_names)
#' of df is in direction relative to size
#'
#' @param df a data.frame (local or remote).
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' @param direction mathematical operator for comparison.
#' @param size numeric value to compare against size.
#' 
#' @return Errors if assert fails. Else, number of distinct values invisibly 
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' assert_num_distinct(iris, "Species", "!=", 1)
#' 
assert_num_distinct = function(df, col_name, direction, size) {
  uu = num_unique_entries(df, col_name)
  
  if(!evaluation_comparison(uu, direction, size)){
    cc = paste0(col_name, collapse = ", ")
    msg = glue::glue("number of uniques in {cc} of df = {uu} is not {direction} {size}")
    stop(msg)
  }
  
  return(invisible(uu))
}

## Check every entry is unique -------------------------------------------- ----
#' Check every entry is distinct
#' 
#' Check whether every value is a column (of across a set of columns) is
#' distinct.
#' 
#' Useful for checking whether a join will be one-to-one. If both tables have
#' every entry distinct on the columns used for the join, then the join will
#' be one-to-one.
#' 
#' @param df a data.frame (local or remote).
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' 
#' @return T/F whether every entry in col_name (or the combination of col_names)
#' is distinct.
#' 
#' @export
#' 
#' @examples
#' # example code
#' 
check_all_unique = function(df, col_name) {
  nn = num_row(df)
  check_num_distinct(df, col_name, "==", nn)
}

## Assert every entry is unique ------------------------------------------- ----
#' Check every entry is distinct
#' 
#' Check whether every value is a column (of across a set of columns) is
#' distinct.
#' 
#' @param df a data.frame (local or remote).
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' 
#' @return Errors if assert fails. Else, number of rows/distinct invisibly.
#' 
#' @export
#'  
assert_all_unique = function(df, col_name) {
  nn = num_row(df)
  uu = num_unique_entries(df, col_name)
  
  if(!evaluation_comparison(uu, "==", nn)){
    cc = paste0(col_name, collapse = ", ")
    msg = glue::glue("not all entries in {cc} of df are unique")
    stop(msg)
  }
  
  return(invisible(nn))
}

## Check number of missing values ----------------------------------------- ----
#' Check number of missing values
#'
#' @param df a data.frame (local or remote).
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' @param direction mathematical operator for comparison.
#' @param size numeric value to compare against size.
#' 
#' @return T/F whether number of missing values in col_name (or cumulative
#' across col_names) of `df` is `direction` relative to `size`.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' check_size_missing(iris, c("Petal.Length", "Petal.Width"), "==", 0)
#' 
check_size_missing = function(df, col_name, direction, size) {
  mm = num_missing_entries(df, col_name)
  return(evaluation_comparison(mm, direction, size))
}

## Assert number of missing values ---------------------------------------- ----
#' Assert number of missing values
#' 
#' @param df a data.frame (local or remote).
#' @param col_name the name(s) of columns to count distinct values/combinations
#' of. Errors if any `col_name` is not a column name of `df`.
#' @param direction mathematical operator for comparison.
#' @param size numeric value to compare against size.
#' 
#' @return Errors if assert fails. Else, number of missing values invisibly.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' assert_size_missing(iris, "Petal.Length", "==", 0)
#' 
assert_size_missing = function(df, col_name, direction, size) {
  mm = num_missing_entries(df, col_name)
  
  if(!evaluation_comparison(mm, direction, size)){
    cc = paste0(col_name, collapse = ", ")
    msg = glue::glue(
      "number of missings in {cc} of df = {mm} is not {direction} {size}"
    )
    stop(msg)
  }
  
  return(invisible(mm))
}

## Check LHS of join is covered by RHS of join ---------------------------- ----
#' Check LHS of join is covered by RHS
#' 
#' When joining two tables, we may want to explicitly test that every record
#' on the left table will be joined to a record on the right table.
#' 
#' Returns T/F whether every value in join_col(s) of df1 is found in df2.
#' If returns TRUE, then no records will be lost during an inner join.
#' 
#' @param df1 a data.frame (local or remote).
#' @param df2 a data.frame (local or remote - must be consistent with `df1`).
#' @param join_col the column(s) to join by. Uses the same syntax as dplyr, so
#' you can use a named vector to join tables with different column names.
#' For example: `join_col = c(col_in_df1 = "col_in_df2")`
#' 
#' @return T/F whether every value in join_col(s) of df1 is found in df2.
#' 
#' @export
#' 
#' @examples
#' data(iris)
#' check_join_covered(iris, iris, join_col = c(Sepal.Width = "Sepal.Length"))
#' 
check_join_covered = function(df1, df2, join_col) {
  stopifnot(is.character(join_col))
  stopifnot(join_col %in% colnames(df2))
  if(is.null(names(join_col))){
    stopifnot(join_col %in% colnames(df1))
  } else {
    stopifnot(names(join_col) %in% colnames(df1))
  }
  
  n1 = num_row(df1)
  n2 = num_row(dplyr::semi_join(df1, df2, by = join_col))
  return(evaluation_comparison(n1, "==", n2))
}

## Assert LHS of join is covered by RHS of join --------------------------- ----
#' Assert LHS of join is covered by RHS
#'
#' When joining two tables, we may want to be sure that every record
#' on the left table will be joined to a record on the right table.
#' 
#' Asserts every value in join_col(s) of `df1` is found in `df2`.
#' If assert passes (without error) then no records in `df1` will be lost during
#' an inner join to `df2`.
#'
#' @param df1 a data.frame (local or remote).
#' @param df2 a data.frame (local or remote - must be consistent with `df1`).
#' @param join_col the column(s) to join by. Uses the same syntax as dplyr, so
#' you can use a named vector to join tables with different column names.
#' For example: `join_col = c(col_in_df1 = "col_in_df2")`
#' 
#' @return Errors if assert fails. Else, number of rows invisibly.
#' 
#' @export
#' 
assert_join_covered = function(df1, df2, join_col) {
  n1 = num_row(df1)
  n2 = num_row(dplyr::semi_join(df1, df2, by = join_col))
  
  if(!evaluation_comparison(n1, "==", n2)){
    msg = glue::glue("{n1 - n2} values in df1 do not appear in df2")
    stop(msg)
  }
  
  return(invisible(n1))
}

## Check date periods do not overlap -------------------------------------- ----
#'
#' Returns T/F for whether any overlap in date columns within groups defined by other columns.
#'
#' If two individuals have date periods that overlap with each other but neither individual
#' as overlapping date periods when considered on their own
#' Then group_by_cols = person_id will return TRUE (no overlap when grouped)
#' but group_by_cols = c() will return FALSE (overlap exists without grouping)
#'
#' @param df a data.frame (local or remote).
#' @param start_date the column containing start dates
#' @param end_date the column containing end dates
#' @param group_by_cols columns to group-by / search within.
#' 
#' @return T/F whether every period does not overlap with any other.
#' 
#' @export
#' 
#' @importFrom rlang .data
check_no_date_overlap = function(df, start_date, end_date, group_by_cols) {
  # checks
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.character(start_date))
  stopifnot(length(start_date) == 1)
  stopifnot(start_date %in% colnames(df))
  stopifnot(is.character(end_date))
  stopifnot(length(end_date) == 1)
  stopifnot(end_date %in% colnames(df))
  
  if (length(group_by_cols) == 0) {
    group_by_cols = "ones_tmp"
    df = dplyr::mutate(df, ones_tmp = 1)
  }
  stopifnot(is.character(group_by_cols))
  stopifnot(all(group_by_cols %in% colnames(df)))
  
  # rename for ease of reference
  df = dplyr::rename(
    df,
    sd_tmp = !!dplyr::sym(start_date),
    ed_tmp = !!dplyr::sym(end_date)
  )
  
  # first check no common start dates
  common_start = dplyr::group_by(df, !!!dplyr::syms(c(group_by_cols, "sd_tmp")))
  common_start = dplyr::summarise(common_start, num_s = dplyr::n())
  common_start = dplyr::filter(common_start, .data$num_s != 1)
  common_start = num_row(common_start)
  
  if (common_start > 0) {
    return(FALSE)
  }
  
  # second check no common end dates
  common_end = dplyr::group_by(df, !!!dplyr::syms(c(group_by_cols, "ed_tmp")))
  common_end = dplyr::summarise(common_end, num_e = dplyr::n())
  common_end = dplyr::filter(common_end, .data$num_e != 1)
  common_end = num_row(common_end)
  
  if (common_end > 0) {
    return(FALSE)
  }
  
  # third check no overlaps when start & end dates do not match
  number_overlaps = dplyr::inner_join(
    df,
    df,
    by = group_by_cols, suffix = c("_x", "_y"),
    relationship = "many-to-many"
  )
  number_overlaps = dplyr::select(
    number_overlaps,
    "sd_tmp_x",
    "sd_tmp_y",
    "ed_tmp_x",
    "ed_tmp_y"
  )
  number_overlaps = dplyr::filter(
    number_overlaps,
    .data$sd_tmp_x <= .data$ed_tmp_y, # overlap check
    .data$sd_tmp_y <= .data$ed_tmp_x,
    .data$sd_tmp_x != .data$sd_tmp_y, # start and end dates do not match
    .data$ed_tmp_x != .data$ed_tmp_y
  )
  number_overlaps = num_row(number_overlaps)
  
  return(number_overlaps == 0)
}
