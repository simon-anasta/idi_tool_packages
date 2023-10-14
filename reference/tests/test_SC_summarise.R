###############################################################################
#' Description: Automated tests for summarise and confidentialise functions
#'
#' Input: summary_confidential.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package, utility_functions.R
#'
#' Notes:
#'
#' Issues:
#'
#' History (reverse order):
#' 2021-09-13 SA v0
#' #############################################################################

#' Testing the following functions that summarise output
#' 
#' summarise_and_label(df, group_by_cols, summarise_col,
#'                     make_distinct, make_count, make_sum,
#'                     clean = "none", remove.na.from.groups = TRUE)
#' summarise_and_label_over_lists(df, group_by_list, summarise_list,
#'                                make_distinct, make_count, make_sum,
#'                                clean = "none", remove.na.from.groups = TRUE)
#'                                
context("summary confidential - summarise")

###############################################################################


###############################################################################

