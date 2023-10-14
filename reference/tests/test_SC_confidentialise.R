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
#' 2021-09-14 SA v0
#' #############################################################################

#' Testing the following functions that confidentialise output
#'
#' apply_random_rounding(df, RR_columns, BASE = 3, stable_across_cols = NULL)
#' apply_graduated_random_rounding(df, GRR_columns, stable_across_cols = NULL)
#' apply_small_count_suppression(df, suppress_cols, threshold, count_cols = suppress_cols)
#' confidentialise_results(df, stable_RR, sum_RR, BASE, COUNT_THRESHOLD, SUM_THRESHOLD)
#' 
context("summary confidential - confidentialise")

#####################################################################


#####################################################################
# apply_graduated_random_rounding(df, GRR_columns, stable_across_cols = NULL)



#####################################################################
# apply_small_count_suppression(df, suppress_cols, threshold, count_cols = suppress_cols)


#####################################################################
# confidentialise_results(df, stable_RR, sum_RR, BASE, COUNT_THRESHOLD, SUM_THRESHOLD)

