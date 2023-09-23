###############################################################################
#' Description: Automated tests for general assembly tool help functions.
#'
#' Input: general_assembly_tool_functions.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package
#'
#' Notes:
#' - Some functions (and tests) assume SQL Server environment
#'   due to differences in syntax between different implementations of SQL.
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-01-06 SA v1
#' 2019-12-16 SA v0
#' #############################################################################

#' Testing the following functions that handle text for SQL
#'
#' read_table_file(file_name_and_path)
#' handle_summary_case(summary_type, proportional, m_label, m_value,
#'                     m_start_date, m_end_date, p_start_date, p_end_date)
#'
context("assembler - data handling")

test_that("all file formats read", {
  data(mtcars)
  write.table(mtcars, "test.csv", sep = ",", row.names = FALSE)
  xlsx::write.xlsx2(mtcars, "test.xls", row.names = FALSE)
  xlsx::write.xlsx2(mtcars, "test.xlsx", row.names = FALSE)

  csv_read <- read_table_file("test.csv")
  xls_read <- read_table_file("test.xls")
  xlsx_read <- read_table_file("test.xlsx")

  unlink("test.csv")
  unlink("test.xls")
  unlink("test.xlsx")

  expect_true(all_equal(csv_read, mtcars, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  expect_true(all_equal(xls_read, mtcars, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  expect_true(all_equal(xlsx_read, mtcars, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

test_that("tool delimiated formats read", {
  df <- data.frame(
    sql = c("[foo]", "[bar]"),
    txt = c('"qwe"', '"asd"'),
    stringsAsFactors = FALSE
  )


  write.table(df, "test.csv", sep = ",", row.names = FALSE, qmethod = "double")
  xlsx::write.xlsx2(df, "test.xls", row.names = FALSE)
  xlsx::write.xlsx2(df, "test.xlsx", row.names = FALSE)

  csv_read <- read_table_file("test.csv")
  xls_read <- read_table_file("test.xls")
  xlsx_read <- read_table_file("test.xlsx")

  expect_true(all_equal(csv_read, df, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  expect_true(all_equal(xls_read, df, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  expect_true(all_equal(xlsx_read, df, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))

  unlink("test.csv")
  unlink("test.xls")
  unlink("test.xlsx")
})
