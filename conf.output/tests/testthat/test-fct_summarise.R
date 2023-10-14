################################################################################
# Description: Automated tests for summarise functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## cross_product_column_names(..., always, drop.dupes.within, drop.dupes.across) ----

test_that("cross product outputs", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  out_manual = list(c("1","4"),c("1","5"),c("2","4"),c("2","5"),c("3","4"),c("3","5"))
  out1 = cross_product_column_names(in1,in2)
  out2 = cross_product_column_names(in2,in1)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_manual)
  expect_setequal(out2, out_manual)
})

test_that("always appears in all products", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  in3 = c("6")
  
  out_manual = list(c("1","4","6"),c("1","5","6"),c("2","4","6"),c("2","5","6"),c("3","4","6"),c("3","5","6"))
  out1 = cross_product_column_names(in1,in2, in3)
  out2 = cross_product_column_names(in1,in2, always = in3)
  out1 = lapply(out1, sort)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_manual)
  expect_setequal(out2, out_manual)
  
  out_always = list(c("1","4","5"),c("2","4","5"),c("3","4","5"))
  out1 = cross_product_column_names(in1, always = in2)
  out2 = cross_product_column_names(always = in2, in1)
  out1 = lapply(out1, sort)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_always)
  expect_setequal(out2, out_always)
})

test_that("product removes dupes within", {
  in1 = c("a","b")
  in2 = c("a","c")
  out_dupes_manual = list(c("a","a"),c("a","b"),c("a","c"),c("b","c"))
  out_no_dupes_manual = list(c("a"),c("a","b"),c("a","c"),c("b","c"))
  
  out_dupes = cross_product_column_names(in1, in2, drop.dupes.within = FALSE)
  out_no_dupes = cross_product_column_names(in1, in2, drop.dupes.within = TRUE)
  out_dupes = lapply(out_dupes, sort)
  out_no_dupes = lapply(out_no_dupes, sort)
  
  expect_setequal(out_dupes, out_dupes_manual)
  expect_setequal(out_no_dupes, out_no_dupes_manual)
})

test_that("product removes dupes across", {
  in1 = c("a","b")
  out_dupes_manual = list(c("a","a"),c("a","b"),c("b","a"),c("b","b"))
  out_no_dupes_within_manual = list(c("a"),c("a","b"),c("b","a"),c("b"))
  out_no_dupes_across_manual = list(c("a","a"),c("a","b"),c("b","b"))
  out_no_dupes_both_manual = list(c("a"),c("a","b"),c("b"))
  
  out_dupes = cross_product_column_names(in1, in1, drop.dupes.within = FALSE, drop.dupes.across = FALSE)
  out_no_dupes_within = cross_product_column_names(in1, in1, drop.dupes.within = TRUE, drop.dupes.across = FALSE)
  out_no_dupes_across = cross_product_column_names(in1, in1, drop.dupes.within = FALSE, drop.dupes.across = TRUE)
  out_no_dupes_both = cross_product_column_names(in1, in1, drop.dupes.within = TRUE, drop.dupes.across = TRUE)
  
  expect_setequal(out_dupes, out_dupes_manual)
  expect_setequal(out_no_dupes_within, out_no_dupes_within_manual)
  expect_setequal(out_no_dupes_across, out_no_dupes_across_manual)
  expect_setequal(out_no_dupes_both, out_no_dupes_both_manual)
})

## has_long_thin_format(df) ----------------------------------------------- ----
# acceptable formats for long-thin:
# col01 val01 ... col99 val99 summarised_var distinct count sum
# col01 val01 ... col99 val99 summarised_var raw_distinct raw_count raw_sum conf_distinct conf_count conf_sum

test_that("long thin format pases",{
  # arrange
  df1 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   col02 = 1:5,
                   val02 = 1:5,
                   summarised_var = 1:5,
                   distinct = 1:5,
                   count = 1:5,
                   sum = 1:5)
  df2 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   col02 = 1:5,
                   val02 = 1:5,
                   summarised_var = 1:5,
                   raw_distinct = 1:5,
                   raw_count = 1:5,
                   raw_sum = 1:5,
                   conf_distinct = 1:5,
                   conf_count = 1:5,
                   conf_sum = 1:5)
  # act
  # assert
  expect_true(has_long_thin_format(df1))
  expect_true(has_long_thin_format(df2))
  expect_true(has_long_thin_format(df1[,3:6]))
  expect_true(has_long_thin_format(df2[,1:7]))
  expect_true(has_long_thin_format(df1[,1:4]))
  expect_true(has_long_thin_format(df2[,10:11]))
  expect_true(has_long_thin_format(df2[,sample(1:11)]))
})

test_that("other formats fail", {
  expect_error(has_long_thin_format(list(1,2,3)), "data\\.frame")
  
  df1 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   col02 = 1:5,
                   summarised_var = 1:5,
                   count = 1:5)
  expect_false(has_long_thin_format(df1))
  
  df2 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   not_a_column_name = 1:5,
                   summarised_var = 1:5,
                   count = 1:5)
  expect_false(has_long_thin_format(df2))
})

## summarise_and_label(df,group_by_cols,summarise_col,make_distinct,make_count,make_sum,clean,remove.na.from.groups,query_path) ----

test_that("individual summarise runs", {
  # arrange
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  expected_output_rownum = tibble::tibble(
    col01 = rep("your_label", 4),
    val01 = c("z","y","z","y"),
    col02 = rep("my_label", 4),
    val02 = c("a","a","b","b"),
    summarised_var = rep("rownum", 4),
    distinct = rep(25.0, 4),
    count = rep(25.0, 4),
    sum = c(625.0, 1875.0, 650.0, 1900.0)
  )
  
  # act
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "rownum",
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  # assert
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(expected_output_rownum, col01, val01, col02, val02)
  ))
})

test_that("individual summarise type control trigger", {
  # arrange
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  expected_output_rownum = tibble::tibble(
    col01 = rep("your_label", 4),
    val01 = c("z","y","z","y"),
    col02 = rep("my_label", 4),
    val02 = c("a","a","b","b"),
    summarised_var = rep("rownum", 4),
    distinct = rep(25.0, 4),
    count = rep(25.0, 4),
    sum = c(625.0, 1875.0, 650.0, 1900.0)
  )
  
  # act & assert
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "rownum",
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  # assert
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(expected_output_rownum, col01, val01, col02, val02)
  ))
  
  
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "rownum",
    make_distinct = FALSE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  this_expect = dplyr::select(expected_output_rownum, -distinct)
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(this_expect, col01, val01, col02, val02)
  ))
  
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "rownum",
    make_distinct = TRUE,
    make_count = FALSE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  this_expect = dplyr::select(expected_output_rownum, -count)
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(this_expect, col01, val01, col02, val02)
  ))
  
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "rownum",
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = FALSE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  this_expect = dplyr::select(expected_output_rownum, -sum)
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(this_expect, col01, val01, col02, val02)
  ))
})

test_that("individual summarise NA controls trigger", {
  # arrange
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  expected_output_none = tibble::tibble(
    col01 = rep("your_label", 4),
    val01 = c("z","y","z","y"),
    col02 = rep("my_label", 4),
    val02 = c("a","a","b","b"),
    summarised_var = rep("values", 4),
    distinct = rep(3.0, 4),
    count = rep(20.0, 4),
    sum = rep(15.0, 4)
  )
  expected_output_NA = tibble::tibble(
    col01 = rep("your_label", 4),
    val01 = c("z","y","z","y"),
    col02 = rep("my_label", 4),
    val02 = c("a","a","b","b"),
    summarised_var = rep("values", 4),
    distinct = rep(2.0, 4),
    count = rep(10.0, 4),
    sum = rep(15.0, 4)
  )
  expected_output_zero = tibble::tibble(
    col01 = rep("your_label", 4),
    val01 = c("z","y","z","y"),
    col02 = rep("my_label", 4),
    val02 = c("a","a","b","b"),
    summarised_var = rep("values", 4),
    distinct = rep(3.0, 4),
    count = rep(25.0, 4),
    sum = rep(15.0, 4)
  )
  
  # act & assert
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "values",
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  # assert
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(expected_output_none, col01, val01, col02, val02)
  ))

  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "values",
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "zero.as.na",
    remove.na.from.groups = TRUE
  )
  # assert
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(expected_output_NA, col01, val01, col02, val02)
  ))

  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = c("your_label", "my_label"),
    summarise_col = "values",
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "na.as.zero",
    remove.na.from.groups = TRUE
  )
  # assert
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01, col02, val02),
    dplyr::arrange(expected_output_zero, col01, val01, col02, val02)
  ))
})

test_that("individual summarise remove NA from groups", {
  # arrange
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  expected_output_NA = tibble::tibble(
    col01 = rep("values", 4),
    val01 = c("0","1","2",NA),
    summarised_var = rep("rownum", 4),
    sum = c(1960.0,1030.0,1050.0,1010.0)
  )
  expected_output_none = dplyr::filter(expected_output_NA, !is.na(val01))
  
  # act & assert
  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = "values",
    summarise_col = "rownum",
    make_distinct = FALSE,
    make_count = FALSE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01),
    dplyr::arrange(expected_output_none, col01, val01)
  ))

  this_actual_output = summarise_and_label(
    df = input_df,
    group_by_cols = "values",
    summarise_col = "rownum",
    make_distinct = FALSE,
    make_count = FALSE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = FALSE
  )
  expect_true(all.equal(
    dplyr::arrange(this_actual_output, col01, val01),
    dplyr::arrange(expected_output_NA, col01, val01)
  ))
})

test_that("input checks stop execution", {
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  
  expect_error(summarise_and_label("input_df", "your_label", "rownum", TRUE, TRUE, TRUE, "none"), "data\\.frame")
  expect_error(summarise_and_label(input_df, 1, "rownum", TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, list("your_label"), "rownum", TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, "not_col", "rownum", TRUE, TRUE, TRUE, "none"), "colnames")
  expect_error(summarise_and_label(input_df, "your_label", 1, TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, "your_label", list("rownum"), TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, "your_label", c("rownum", "values"), TRUE, TRUE, TRUE, "none"), "length")
  expect_error(summarise_and_label(input_df, "your_label", "not_col", TRUE, TRUE, TRUE, "none"), "colnames")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", TRUE, TRUE, TRUE, "other"), "clean")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", "T", TRUE, TRUE, "none"), "logical")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", TRUE, "T", TRUE, "none"), "logical")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", TRUE, TRUE, "T", "none"), "logical")
})

## summarise_and_label_over_lists(df, group_by_list,summarise_list,make_distinct,make_count,make_sum,clean,remove.na.from.groups,query_path) ----

test_that("multi summarise runs", {
  # arrange
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  
  # act
  group_by_list = list("your_label", "my_label")
  summarise_list = list("rownum", "values")
  actual_output = summarise_and_label_over_lists(
    df = input_df,
    group_by_list,
    summarise_list,
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  expected_output = dplyr::bind_rows(
    summarise_and_label(
      df = input_df,
      group_by_list[[1]],
      summarise_list[[1]],
      make_distinct = TRUE,
      make_count = TRUE,
      make_sum = TRUE,
      clean = "none",
      remove.na.from.groups = TRUE
    ),
    summarise_and_label(
      df = input_df,
      group_by_list[[2]],
      summarise_list[[1]],
      make_distinct = TRUE,
      make_count = TRUE,
      make_sum = TRUE,
      clean = "none",
      remove.na.from.groups = TRUE
    ),
    summarise_and_label(
      df = input_df,
      group_by_list[[1]],
      summarise_list[[2]],
      make_distinct = TRUE,
      make_count = TRUE,
      make_sum = TRUE,
      clean = "none",
      remove.na.from.groups = TRUE
    ),
    summarise_and_label(
      df = input_df,
      group_by_list[[2]],
      summarise_list[[2]],
      make_distinct = TRUE,
      make_count = TRUE,
      make_sum = TRUE,
      clean = "none",
      remove.na.from.groups = TRUE
    )
  )
  
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
})

test_that("multi summarise passes controls through",{
  # arrange
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  
  # act
  group_by_list = list("my_label")
  summarise_list = list("values")
  actual_output = summarise_and_label_over_lists(
    df = input_df,
    group_by_list,
    summarise_list,
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  expected_output = summarise_and_label(
    df = input_df,
    group_by_list[[1]],
    summarise_list[[1]],
    make_distinct = TRUE,
    make_count = TRUE,
    make_sum = TRUE,
    clean = "none",
    remove.na.from.groups = TRUE
  )
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
  
  # act - make_distinct
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, FALSE, TRUE, TRUE, "none", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], FALSE, TRUE, TRUE, "none", TRUE)
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
  
  # act - make_count
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, FALSE, TRUE, "none", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, FALSE, TRUE, "none", TRUE)
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
  
  # act - make_sum
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, TRUE, FALSE, "none", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, TRUE, FALSE, "none", TRUE)
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
  
  # act - na.as.zero
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, TRUE, TRUE, "na.as.zero", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, TRUE, TRUE, "na.as.zero", TRUE)
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
  
  # act - zero.as.na
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, TRUE, TRUE, "zero.as.na", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, TRUE, TRUE, "zero.as.na", TRUE)
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
  
  # act - remove.na.from.groups
  actual_output = summarise_and_label_over_lists(input_df, list("values"), list("rownum"), TRUE, TRUE, TRUE, "zero.as.na", FALSE)
  expected_output = summarise_and_label(input_df, "values", "rownum", TRUE, TRUE, TRUE, "zero.as.na", FALSE)
  # assert
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
})

test_that("input checks stop execution", {
  input_df = tibble::tibble(
    your_label = c(rep("z",50), rep("y",50)),
    my_label = rep(c("a","b"), 50),
    rownum = 1:100,
    values = rep(c(0,0,NA,1,2), 20)
  )
  group_by_list = list("your_label", "my_label")
  summarise_list = list("rownum", "values")
  
  # expect_error(filter_to_limited_number_of_rows(rep(1:100)), "data\\.frame")
  expect_error(summarise_and_label_over_lists("input_df", group_by_list, summarise_list, TRUE, TRUE, TRUE, "none"), "data\\.frame")
  expect_error(summarise_and_label_over_lists(input_df, "group_by_list", summarise_list, TRUE, TRUE, TRUE, "none"), "list")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, "summarise_list", TRUE, TRUE, TRUE, "none"), "list")
  expect_error(summarise_and_label_over_lists(input_df, list(), summarise_list, TRUE, TRUE, TRUE, "none"), "length")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, list(), TRUE, TRUE, TRUE, "none"), "length")
  expect_error(summarise_and_label_over_lists(input_df, list("my_label", 2), summarise_list, TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, list("rownum", 3), TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label_over_lists(input_df, list("my_label", "not col"), summarise_list, TRUE, TRUE, TRUE, "none"), "colnames")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, list("rownum", "not col"), TRUE, TRUE, TRUE, "none"), "colnames")
})

test_that("different input types merge", {
  input_df = tibble::tibble(
    c1 = c("a","b","a","b"),
    c2 = c(1.0, 1.0, 2.5, 2.5),
    c3 = c(9, 8, 7, 6)
  )
  group_by_list = list("c1", "c2")
  summarise_list = list("c3")
  
  expected_output = tibble::tibble(
    col01 = c("c1", "c1", "c2", "c2"),
    val01 = c("a", "b", "1", "2.5"),
    summarised_var = c("c3", "c3", "c3", "c3"),
    sum = c(16, 14, 17, 13)
  )
  
  actual_output = summarise_and_label_over_lists(
    df = input_df,
    group_by_list = group_by_list,
    summarise_list = summarise_list,
    make_distinct = FALSE,
    make_count = FALSE,
    make_sum = TRUE
  )
  
  expect_true(all.equal(
    dplyr::arrange(actual_output, col01, val01),
    dplyr::arrange(expected_output, col01, val01)
  ))
})
