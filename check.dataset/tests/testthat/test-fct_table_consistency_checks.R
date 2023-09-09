################################################################################
# Description: Automated tests for table consistency functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## evaluation_comparison(val1, direction, val2) --------------------------- ----

test_that("every direction works", {
  expect_true(evaluation_comparison(1, "==", 1))
  expect_true(evaluation_comparison(0, "<=", 1))
  expect_true(evaluation_comparison(0, "<", 1))
  expect_true(evaluation_comparison(2, ">=", 1))
  expect_true(evaluation_comparison(2, ">", 1))
  expect_true(evaluation_comparison(2, "!=", 1))
  
  expect_false(evaluation_comparison(1, "==", 2))
  expect_false(evaluation_comparison(1, "<=", 0))
  expect_false(evaluation_comparison(1, "<", 0))
  expect_false(evaluation_comparison(1, ">=", 2))
  expect_false(evaluation_comparison(1, ">", 2))
  expect_false(evaluation_comparison(1, "!=", 1))
})

test_that("invalid input is caught (with error type)", {
  expect_error(evaluation_comparison(1, 1, 1), "direction")
  expect_error(evaluation_comparison(1, NA, 1), "direction")
  expect_error(evaluation_comparison(1, "==", NA), "val2")
  expect_error(evaluation_comparison(1, "==", NA), "numeric")
  expect_error(evaluation_comparison(NA, "==", 1), "val1")
  expect_error(evaluation_comparison(NA, "==", 1), "numeric")
})

## num_row(df) ------------------------------------------------------------ ----

test_that("different ways to get size are equivalent", {
  data(mtcars)
  
  expect_equal(num_row(mtcars), nrow(mtcars))
  expect_equal(num_row(mtcars), length(mtcars[, 1]))
})

test_that("invalid input is caught (with error type)", {
  expect_error(num_row(1), "data.frame")
  expect_error(num_row("s"), "data.frame")
  expect_error(num_row(list("s")), "data.frame")
})

## check_size(df, direction, size) ---------------------------------------- ----

test_that("size is checked", {
  data(mtcars)
  nn = nrow(mtcars)
  
  expect_true(check_size(mtcars, "==", nn))
  expect_false(check_size(mtcars, "!=", nn))
  expect_true(check_size(mtcars, ">=", 1))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(check_size(mtcars, "==", NA), "numeric")
  expect_error(check_size(mtcars, "", 2), "direction")
  expect_error(check_size("mtcars", "==", 2), "data.frame")
})

## assert_size(df, direction, size) --------------------------------------- ----

test_that("size is asserted", {
  data(mtcars)
  nn = nrow(mtcars)
  
  expect_silent(assert_size(mtcars, "==", nn))
  expect_error(assert_size(mtcars, "!=", nn))
  expect_silent(assert_size(mtcars, ">=", 1))
  
  # required compoents of error message are present
  expect_error(assert_size(mtcars, "!=", nn), "!=")
  expect_error(assert_size(mtcars, "!=", nn), as.character(nn))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(assert_size(mtcars, "==", NA), "numeric")
  expect_error(assert_size(mtcars, "", 2), "direction")
  expect_error(assert_size("mtcars", "==", 2), "data.frame")
})

## check_size_comparison(df1, direction, df2) ----------------------------- ----

test_that("size comparison is checked", {
  data(mtcars)
  mtcars2 = dplyr::filter(mtcars, mpg <= 20)
  
  expect_true(check_size_comparison(mtcars, ">=", mtcars2))
  expect_true(check_size_comparison(mtcars, "!=", mtcars2))
  expect_true(check_size_comparison(mtcars, "==", mtcars))
  
  expect_false(check_size_comparison(mtcars, "==", mtcars2))
  expect_false(check_size_comparison(mtcars, "<", mtcars2))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(check_size_comparison("mtcars", "==", mtcars), "data.frame")
  expect_error(check_size_comparison(mtcars, "==", "mtcars"), "data.frame")
  expect_error(check_size_comparison(mtcars, "===", mtcars), "direction")
})

## assert_size_comparison(df1, direction, df2) ---------------------------- ----

test_that("size comparison is asserted", {
  data(mtcars)
  mtcars2 = dplyr::filter(mtcars, mpg <= 20)
  n1 = nrow(mtcars)
  n2 = nrow(mtcars2)
  
  expect_silent(assert_size_comparison(mtcars, ">=", mtcars2))
  expect_silent(assert_size_comparison(mtcars, "!=", mtcars2))
  expect_silent(assert_size_comparison(mtcars, "==", mtcars))
  
  expect_error(assert_size_comparison(mtcars, "==", mtcars2))
  expect_error(assert_size_comparison(mtcars, "<", mtcars2))
  
  # required compoents of error message are present
  expect_error(assert_size_comparison(mtcars, "==", mtcars2), "==")
  expect_error(assert_size_comparison(mtcars, "==", mtcars2), as.character(n1))
  expect_error(assert_size_comparison(mtcars, "==", mtcars2), as.character(n2))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(assert_size_comparison("mtcars", "==", mtcars), "data.frame")
  expect_error(assert_size_comparison(mtcars, "==", "mtcars"), "data.frame")
  expect_error(assert_size_comparison(mtcars, "===", mtcars), "direction")
})

## num_unique_entries(df, col_name) --------------------------------------- ----

test_that("different ways to check number of uniques are equivalent", {
  data(mtcars)
  
  expect_equal(num_unique_entries(mtcars, "mpg"), dplyr::n_distinct(mtcars$mpg))
  expect_equal(num_unique_entries(mtcars, "mpg"), length(unique(mtcars$mpg)))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(num_unique_entries("mtcars", "gear"), "data.frame")
  expect_error(num_unique_entries(mtcars, 1), "character")
  expect_error(num_unique_entries(mtcars, "made_up_column_name"), "colnames")
})

test_that("multiple columns can be input for unique and join", {
  data(mtcars)
  cols = c("gear", "am")
  
  expect_equal(num_unique_entries(mtcars, cols), dplyr::n_distinct(mtcars[, cols]))
})

## check_num_distinct(df, col_name, direction, size) ---------------------- ----

test_that("number of uniques is checked", {
  data(mtcars)
  
  expect_true(check_num_distinct(mtcars, "gear", "==", 3))
  expect_false(check_num_distinct(mtcars, "gear", "==", 2))
  expect_true(check_num_distinct(mtcars, "mpg", ">", 1))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(check_num_distinct("mtcars", "gear", "==", 3), "data.frame")
  expect_error(check_num_distinct(mtcars, "made_up_column_name", "==", 3), "colnames")
  expect_error(check_num_distinct(mtcars, "gear", "wrong", 3), "direction")
  expect_error(check_num_distinct(mtcars, "gear", "==", "2"), "numeric")
})

test_that("multiple columns can be input for unique and join", {
  data(mtcars)
  cols = c("gear", "am")
  
  expect_true(check_num_distinct(mtcars, cols, "==", 4))
})

## assert_num_distinct(df, col_name, direction, size) --------------------- ----

test_that("number of uniques is asserted", {
  data(mtcars)
  
  expect_silent(assert_num_distinct(mtcars, "gear", "==", 3))
  expect_silent(assert_num_distinct(mtcars, "mpg", ">", 1))
  expect_error(assert_num_distinct(mtcars, "gear", "==", 2))
  
  # required compoents of error message are present
  expect_error(assert_num_distinct(mtcars, "gear", "==", 2), "==")
  expect_error(assert_num_distinct(mtcars, "gear", "==", 2), "2")
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(assert_num_distinct("mtcars", "gear", "==", 3), "data.frame")
  expect_error(assert_num_distinct(mtcars, "made_up_column_name", "==", 3), "colnames")
  expect_error(assert_num_distinct(mtcars, "gear", "wrong", 3), "direction")
  expect_error(assert_num_distinct(mtcars, "gear", "==", "2"), "numeric")
})

test_that("multiple columns can be input for unique and join", {
  data(mtcars)
  cols = c("gear", "am")
  
  expect_error(assert_num_distinct(mtcars, cols, "!=", 4), "gear, am")
})

## check_all_unique(df, col_name) ----------------------------------------- ----

test_that("all uniques is checked", {
  my_data = data.frame(uu = c(1, 2, 3, 4, 5, 6), vv = c("a", "b", "a", "b", "c", "b"))
  
  expect_true(check_all_unique(my_data, "uu"))
  expect_false(check_all_unique(my_data, "vv"))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(check_all_unique("mtcars", "gear"), "data.frame")
  expect_error(check_all_unique(mtcars, "made_up_column_name"), "colnames")
})

test_that("multiple columns can be input for unique and join", {
  data(mtcars)
  cols = c("gear", "am")
  
  expect_false(check_all_unique(mtcars, cols))
})

## assert_all_unique(df, col_name) ---------------------------------------- ----

test_that("all uniques is asserted", {
  my_data = data.frame(uu = c(1, 2, 3, 4, 5, 6), vv = c("a", "b", "a", "b", "c", "b"))
  
  expect_silent(assert_all_unique(my_data, "uu"))
  expect_error(assert_all_unique(my_data, "vv"), "vv")
  expect_error(assert_all_unique(my_data, "vv"), "not all entries")
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(assert_all_unique("mtcars", "gear"), "data.frame")
  expect_error(assert_all_unique(mtcars, "made_up_column_name"), "colnames")
})

test_that("multiple columns can be input for unique and join", {
  data(mtcars)
  cols = c("gear", "am")
  
  expect_error(assert_all_unique(mtcars, cols), "gear, am")
})

## check_join_covered(df1, df2, join_col) --------------------------------- ----

test_that("join coverage is checked", {
  df1 = data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 2, 3, 1, 2))
  df2 = data.frame(a = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))
  
  expect_true(check_join_covered(df2, df1, "a"))
  expect_false(check_join_covered(df1, df2, "a"))
  expect_true(check_join_covered(df2, df1, c(z = "b")))
  expect_false(check_join_covered(df1, df2, c(b = "z")))
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(check_join_covered("mtcars", mtcars, "mpg"))
  expect_error(check_join_covered(mtcars, "mtcars", "mpg"))
  expect_error(check_join_covered(mtcars, mtcars, "made_up_column_name"))
})

test_that("multiple columns can be input for unique and join", {
  df1 = data.frame(a = c(1, 2, 3, 4, 5, 4), b = c(1, 2, 3, 1, 2, 2), a2 = c(3, 3, 2, 3, 2, 2))
  df2 = data.frame(y = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))
  
  expect_true(check_join_covered(df2, df1, c(y = "a", z = "a2")))
  expect_false(check_join_covered(df1, df2, c(a = "y", b = "z")))
})

## assert_join_covered(df1, df2, join_col) -------------------------------- ----

test_that("join coverage is asserted", {
  df1 = data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 2, 3, 1, 2))
  df2 = data.frame(a = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))
  
  expect_silent(assert_join_covered(df2, df1, "a"))
  expect_silent(assert_join_covered(df2, df1, c(z = "b")))
  
  expect_error(assert_join_covered(df1, df2, "a"), "1 values")
  expect_error(assert_join_covered(df1, df2, c(b = "z")), "2 values")
})

test_that("invalid input is caught (with error type)", {
  data(mtcars)
  
  expect_error(assert_join_covered("mtcars", mtcars, "mpg"))
  expect_error(assert_join_covered(mtcars, "mtcars", "mpg"))
  expect_error(assert_join_covered(mtcars, mtcars, "made_up_column_name"))
})

test_that("multiple columns can be input for unique and join", {
  df1 = data.frame(a = c(1, 2, 3, 4, 5, 4), b = c(1, 2, 3, 1, 2, 2), a2 = c(3, 3, 2, 3, 2, 2))
  df2 = data.frame(y = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))
  
  expect_error(assert_join_covered(df1, df2, c(a = "y", b = "z")), "5 values")
})

## num_missing_entries(df, col_name) -------------------------------------- ----

test_that("correct number of missings", {
  tmp = data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))
  
  expect_equal(num_missing_entries(tmp, "a"), 3)
  expect_equal(num_missing_entries(tmp, "b"), 2)
  expect_equal(num_missing_entries(tmp, c("a", "b")), 5)
})

test_that("invalid input is caught (with error type)", {
  tmp = data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))
  
  expect_error(num_missing_entries("tmp", "a"), "data.frame")
  expect_error(num_missing_entries(tmp, 1), "character")
  expect_error(num_missing_entries(tmp, "made_up_column_name"), "colnames")
})

## check_size_missing(df, col_name, direction, size) ---------------------- ----

test_that("number of missings is checked", {
  tmp = data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))
  
  expect_true(check_size_missing(tmp, "a", "==", 3))
  expect_false(check_size_missing(tmp, "a", "==", 2))
  expect_true(check_size_missing(tmp, "b", ">=", 1))
  expect_true(check_size_missing(tmp, c("a", "b"), "==", 5))
})

test_that("invalid input is caught (with error type)", {
  tmp = data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))
  
  expect_error(check_size_missing("tmp", "a", "==", 3), "data.frame")
  expect_error(check_size_missing(tmp, "made_up_column_name", "==", 3), "colnames")
  expect_error(check_size_missing(tmp, "a", "wrong", 3), "direction")
  expect_error(check_size_missing(tmp, "a", "==", "2"), "numeric")
})

## assert_size_uniques(df, col_name, direction, size) --------------------- ----

test_that("number of missings is asserted", {
  tmp = data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))
  
  expect_silent(assert_size_missing(tmp, "a", "==", 3))
  expect_error(assert_size_missing(tmp, "a", "==", 2))
  expect_silent(assert_size_missing(tmp, "b", ">=", 1))
  expect_silent(assert_size_missing(tmp, c("a", "b"), "==", 5))
  
  # required compoents of error message are present
  expect_error(assert_size_missing(tmp, "a", "==", 2), "==")
  expect_error(assert_size_missing(tmp, "a", "==", 2), "2")
})

test_that("invalid input is caught (with error type)", {
  tmp = data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))
  
  expect_error(assert_size_missing("tmp", "a", "==", 3), "data.frame")
  expect_error(assert_size_missing(tmp, "made_up_column_name", "==", 3), "colnames")
  expect_error(assert_size_missing(tmp, "a", "wrong", 3), "direction")
  expect_error(assert_size_missing(tmp, "a", "==", "2"), "numeric")
})

## check_no_date_overlap(df, start_date, end_date, group_by_cols) --------- ----

test_that("date overlap is checked", {
  df = data.frame(
    id = c(1, 1, 2, 2),
    sd = as.Date(c("2010-01-01", "2011-01-01", "2013-01-01", "2014-01-01")),
    ed = as.Date(c("2010-06-06", "2015-01-01", "2013-06-06", "2014-06-06")),
    extra = c("a", "b", "d", "b")
  )
  
  expect_false(check_no_date_overlap(df, "sd", "ed", c()))
  expect_false(check_no_date_overlap(df, "sd", "ed", "extra"))
  
  expect_true(check_no_date_overlap(df, "sd", "ed", "id"))
  expect_true(check_no_date_overlap(df, "sd", "ed", c("id", "extra")))
  
  # duplicate dates
  df = data.frame(
    id = c(1, 1, 2, 2),
    sd = as.Date(c("2010-01-01", "2011-01-01", "2010-01-01", "2014-01-01")),
    ed = as.Date(c("2010-06-06", "2011-01-01", "2010-06-06", "2014-06-06"))
  )
  
  expect_false(check_no_date_overlap(df, "sd", "ed", c()))
  expect_true(check_no_date_overlap(df, "sd", "ed", "id"))
  
  # start_date = end_date
  df = data.frame(
    sd = as.Date(c("2010-01-01", "2010-06-06")),
    ed = as.Date(c("2010-06-06", "2011-01-01"))
  )
  
  expect_false(check_no_date_overlap(df, "sd", "ed", c()))
})

test_that("invalid input is caught (with error type)", {
  df = data.frame(
    sd = as.Date(c("2010-01-01", "2010-06-06")),
    ed = as.Date(c("2010-06-06", "2011-01-01"))
  )
  
  expect_error(check_no_date_overlap("df", "sd", "ed", c()), "data.frame")
  expect_error(check_no_date_overlap(df, 1, "ed", c()), "character")
  expect_error(check_no_date_overlap(df, c("sd", "sd"), "ed", c()), "length\\(start_date\\)")
  expect_error(check_no_date_overlap(df, "1", "ed", c()), "colnames")
  expect_error(check_no_date_overlap(df, "sd", 1, c()), "character")
  expect_error(check_no_date_overlap(df, "sd", c("ed", "ed"), c()), "length\\(end_date\\)")
  expect_error(check_no_date_overlap(df, "sd", "1", c()), "colnames")
  
  expect_error(check_no_date_overlap(df, "sd", "ed", 1), "character")
  expect_error(check_no_date_overlap(df, "sd", "ed", "1"), "colnames")
})
