################################################################################
# Description: Automated tests for overview dataset functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## filter_to_limited_number_of_rows(df, row_limit = 10000, id_column = NA)  ----

test_that("filtering of small tables returns in full", {
  # arrange
  start_df = data.frame(
    id = 1:100,
    col1 = rep(1:4, 25),
    col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25))
  )
  
  # act
  out_df1 = filter_to_limited_number_of_rows(start_df, row_limit = 100)
  out_df2 = filter_to_limited_number_of_rows(start_df, row_limit = 101)
  out_df3 = filter_to_limited_number_of_rows(start_df, row_limit = 1000)
  
  # assert
  expect_true(all.equal(start_df, out_df1, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(start_df, out_df2, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(start_df, out_df3, ignore_row_order = TRUE, ignore_col_order = FALSE))
})

test_that("no id returns top rows", {
  # arrange
  start_df = data.frame(
    id = 1:100,
    col1 = rep(1:4, 25),
    col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25))
  )
  
  # act
  out_df1 = filter_to_limited_number_of_rows(start_df, row_limit = 1)
  out_df2 = filter_to_limited_number_of_rows(start_df, row_limit = 10)
  out_df3 = filter_to_limited_number_of_rows(start_df, row_limit = 50)
  
  # assert
  expect_true(all.equal(head(start_df, 1), out_df1, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(head(start_df, 10), out_df2, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(head(start_df, 50), out_df3, ignore_row_order = TRUE, ignore_col_order = FALSE))
})

test_that("filtering occurs by id", {
  # arrange
  ROW_LIMIT = 10
  start_df = data.frame(
    id = 1:100,
    col1 = rep(1:4, 25),
    col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25))
  )
  
  # act
  out_df = filter_to_limited_number_of_rows(start_df, row_limit = ROW_LIMIT, id_column = "id")
  
  out_ids = sort(out_df$id)
  # assert
  expect_false(isTRUE(all.equal(head(start_df, nrow(out_df)), out_df, ignore_row_order = TRUE, ignore_col_order = TRUE)))
  expect_true(min(diff(out_ids)) > ROW_LIMIT / 2)
})

test_that("filtering by id can return different numbers of rows", {
  # arrange
  start_df = data.frame(
    id = 1:100,
    col1 = rep(1:4, 25),
    col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25))
  )
  # act
  out_df1 = filter_to_limited_number_of_rows(start_df, row_limit = 29, id_column = "id")
  out_df2 = filter_to_limited_number_of_rows(start_df, row_limit = 71, id_column = "id")
  out_df3 = filter_to_limited_number_of_rows(start_df, row_limit = 17, id_column = "id")
  # assert
  expect_true(nrow(out_df1) <= 29)
  expect_true(nrow(out_df2) <= 71)
  expect_true(nrow(out_df3) <= 17)
  expect_true(abs(nrow(out_df1) - nrow(out_df2)) > 5)
  expect_true(abs(nrow(out_df3) - nrow(out_df2)) > 5)
  expect_true(abs(nrow(out_df1) - nrow(out_df3)) > 5)
})

test_that("input checks stop execution", {
  start_df = data.frame(
    id = 1:100,
    col1 = rep(1:4, 25),
    col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25))
  )
  
  expect_error(filter_to_limited_number_of_rows(rep(1:100)), "data\\.frame")
  expect_error(filter_to_limited_number_of_rows("start_df"), "data\\.frame")
  expect_error(filter_to_limited_number_of_rows(start_df, "10"), "numeric")
  expect_error(filter_to_limited_number_of_rows(start_df, NA), "numeric")
  expect_error(filter_to_limited_number_of_rows(start_df, 10, 2), "is.character")
  expect_error(filter_to_limited_number_of_rows(start_df, 10, "not_a_column"), "colnames")
})

## explore_report(df, id_column = NA, target = NA, output_file = NA, output_dir = NA) ----

test_that("reports are produced", {
  # arrange
  data(iris)
  start_df = data.frame(
    id = 1:20000,
    col1 = rep(1:4, 5000),
    col2 = sample(c("a","b","c","d","e","f","g"), 20000, replace = TRUE)
  )
  
  # act - limit to three for speed
  iris_simple = explore_report(iris, id_column = NA, target = NA, output_file = "iris_simple")
  df_col1 = explore_report(start_df, id_column = NA, target = "col1", output_file = "df_col1")
  df_id_col2 = explore_report(start_df, id_column = "id", target = "col2", output_file = "df_id_col2")
  
  # assert
  expect_true(iris_simple %in% list.files())
  expect_true(df_col1 %in% list.files())
  expect_true(df_id_col2 %in% list.files())
  
  # tidy
  unlink(c(iris_simple, df_col1, df_id_col2))
})

test_that("input checks stop execution", {
  start_df = data.frame(
    id = 1:200,
    col1 = rep(1:4, 50),
    col2 = sample(c("a","b","c","d","e","f","g"), 200, replace = TRUE)
  )
  
  expect_error(explore_report("start_df"), "data\\.frame")
  expect_error(explore_report(start_df, id_column = 4), "is.character")
  expect_error(explore_report(start_df, target = 4), "is.character")
  expect_error(explore_report(start_df, target = "not_a_column"), "colnames")
  expect_error(explore_report(start_df, output_file = 123), "is.character")
  expect_error(explore_report(start_df, output_dir = 123), "is.character")
})
