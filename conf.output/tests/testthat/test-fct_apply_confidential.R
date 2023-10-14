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

compare_all_equal = function(df1, df2, arrange_cols){
  result = isTRUE(all.equal(
    dplyr::arrange(df1, !!!rlang::syms(arrange_cols)),
    dplyr::arrange(df2, !!!rlang::syms(arrange_cols))
  ))
  return(result)
}

## randomly_round_vector(input_vector, base = 3, seeds = NULL) ------------ ----

test_that("random rounding matches base",{
  # arrange
  input = 1:100
  # act
  rr3 = randomly_round_vector(input, base = 3)
  rr4 = randomly_round_vector(input, base = 4)
  rr7 = randomly_round_vector(input, base = 7)
  # assert
  expect_true(all(rr3 %% 3 == 0))
  expect_true(all(rr4 %% 4 == 0))
  expect_true(all(rr7 %% 7 == 0))
})

test_that("random rounding is within base of original", {
  # arrange
  input = 1:100
  # act
  rr3 = randomly_round_vector(input, base = 3)
  rr4 = randomly_round_vector(input, base = 4)
  rr7 = randomly_round_vector(input, base = 7)
  # assert
  expect_true(max(abs(rr3 - input)) < 3)
  expect_true(max(abs(rr4 - input)) < 4)
  expect_true(max(abs(rr7 - input)) < 7)
})

test_that("random rounding is distributed correctly", {
  # arrange
  LENG = 10000
  input = 1:LENG
  
  # act - base 3
  rr3 = randomly_round_vector(input, base = 3)
  diff3 = abs(rr3 - input)
  diff_of_0 = sum(diff3 == 0)
  diff_of_1 = sum(diff3 == 1)
  diff_of_2 = sum(diff3 == 2)
  denom = diff_of_1 + diff_of_2
  
  # assert - base 3
  expect_true(0.30 < diff_of_0 / LENG & diff_of_0 / LENG < 0.36)
  expect_true(0.63 < diff_of_1 / denom & diff_of_1 / denom < 0.69)
  expect_true(0.30 < diff_of_2 / denom & diff_of_2 / denom < 0.36)
  
  # act - base 7
  rr7 = randomly_round_vector(input, base = 7)
  diff7 = abs(rr7 - input)
  # remainders
  remain_of_0 = sum(input %% 7 == 0)
  remain_of_1 = sum(input %% 7 == 1)
  remain_of_2 = sum(input %% 7 == 2)
  remain_of_3 = sum(input %% 7 == 3)
  remain_of_4 = sum(input %% 7 == 4)
  remain_of_5 = sum(input %% 7 == 5)
  remain_of_6 = sum(input %% 7 == 6)
  # difference for each remainder
  remain0_diff0 = sum(input %% 7 == 0 & diff7 == 0)
  remain1_diff1 = sum(input %% 7 == 1 & diff7 == 1)
  remain1_diff6 = sum(input %% 7 == 1 & diff7 == 6)
  remain2_diff2 = sum(input %% 7 == 2 & diff7 == 2)
  remain2_diff5 = sum(input %% 7 == 2 & diff7 == 5)
  remain3_diff3 = sum(input %% 7 == 3 & diff7 == 3)
  remain3_diff4 = sum(input %% 7 == 3 & diff7 == 4)
  remain4_diff3 = sum(input %% 7 == 4 & diff7 == 3)
  remain4_diff4 = sum(input %% 7 == 4 & diff7 == 4)
  remain5_diff2 = sum(input %% 7 == 5 & diff7 == 2)
  remain5_diff5 = sum(input %% 7 == 5 & diff7 == 5)
  remain6_diff1 = sum(input %% 7 == 6 & diff7 == 1)
  remain6_diff6 = sum(input %% 7 == 6 & diff7 == 6)
  # remainders and difference match
  expect_equal(LENG, remain0_diff0 + remain1_diff1 + remain1_diff6 + remain2_diff2 + remain2_diff5 +
                 remain3_diff3 + remain3_diff4 + remain4_diff3 + remain4_diff4 +
                 remain5_diff2 + remain5_diff5 + remain6_diff1 + remain6_diff6)
  # assert - base 7
  expect_true(1.00 <= remain0_diff0 / remain_of_0 & remain0_diff0 / remain_of_0 <= 1.00)
  expect_true(0.80 <= remain1_diff1 / remain_of_1 & remain1_diff1 / remain_of_1 <= 0.91)
  expect_true(0.09 <= remain1_diff6 / remain_of_1 & remain1_diff6 / remain_of_1 <= 0.20)
  expect_true(0.68 <= remain2_diff2 / remain_of_2 & remain2_diff2 / remain_of_2 <= 0.74)
  expect_true(0.26 <= remain2_diff5 / remain_of_2 & remain2_diff5 / remain_of_2 <= 0.32)
  expect_true(0.54 <= remain3_diff3 / remain_of_3 & remain3_diff3 / remain_of_3 <= 0.61)
  expect_true(0.39 <= remain3_diff4 / remain_of_3 & remain3_diff4 / remain_of_3 <= 0.46)
  expect_true(0.54 <= remain4_diff3 / remain_of_4 & remain4_diff3 / remain_of_4 <= 0.61)
  expect_true(0.39 <= remain4_diff4 / remain_of_4 & remain4_diff4 / remain_of_4 <= 0.46)
  expect_true(0.68 <= remain5_diff2 / remain_of_5 & remain5_diff2 / remain_of_5 <= 0.74)
  expect_true(0.26 <= remain5_diff5 / remain_of_5 & remain5_diff5 / remain_of_5 <= 0.32)
  expect_true(0.80 <= remain6_diff1 / remain_of_6 & remain6_diff1 / remain_of_6 <= 0.91)
  expect_true(0.09 <= remain6_diff6 / remain_of_6 & remain6_diff6 / remain_of_6 <= 0.20)
})

test_that("random rounding can be stable", {
  # arrange
  input = 1:10000
  seeds = 123 + 1:10000
  
  # act
  rr3a = randomly_round_vector(input, base = 3, seeds = seeds)
  rr3b = randomly_round_vector(input, base = 3, seeds = seeds)
  rr3c = randomly_round_vector(input, base = 3)
  
  # assert
  expect_true(all(rr3a == rr3b))
  expect_false(all(rr3a == rr3c))
})

## apply_random_rounding(df, RR_columns, BASE = 3, stable_across_cols = NULL) ----

test_that("new columns created", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df1 = apply_random_rounding(input_df, "count")
  output_df2 = apply_random_rounding(input_df, "distinct")
  output_df3 = apply_random_rounding(input_df, c("distinct", "count"))
  # assert
  expect_true(all(c("raw_count", "conf_count") %in% colnames(output_df1)))
  expect_true(all(c("raw_count", "conf_count") %in% colnames(output_df3)))
  expect_true(all(c("raw_distinct", "conf_distinct") %in% colnames(output_df2)))
  expect_true(all(c("raw_distinct", "conf_distinct") %in% colnames(output_df3)))
})

test_that("random rounding applied", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df = apply_random_rounding(input_df, "count")
  # assert
  expect_true(all(output_df$conf_count %% 3 == 0))
  expect_true(all(abs(output_df$conf_count - output_df$raw_count) < 3))
})

test_that("base change works", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df4 = apply_random_rounding(input_df, "count", BASE = 4)
  output_df7 = apply_random_rounding(input_df, "count", BASE = 7)
  output_df12 = apply_random_rounding(input_df, "count", BASE = 12)
  # assert
  expect_true(all(output_df4$conf_count %% 4 == 0))
  expect_true(all(abs(output_df4$conf_count - output_df4$raw_count) < 4))
  expect_true(all(output_df7$conf_count %% 7 == 0))
  expect_true(all(abs(output_df7$conf_count - output_df7$raw_count) < 7))
  expect_true(all(output_df12$conf_count %% 12 == 0))
  expect_true(all(abs(output_df12$conf_count - output_df12$raw_count) < 12))
})

test_that("varying columns works", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df = apply_random_rounding(input_df, c("distinct","count","sum"))
  # assert
  expect_true(all(output_df$conf_distinct %% 3 == 0))
  expect_true(all(abs(output_df$conf_distinct - output_df$raw_distinct) < 3))
  expect_true(all(output_df$conf_count %% 3 == 0))
  expect_true(all(abs(output_df$conf_count - output_df$raw_count) < 3))
  expect_true(all(output_df$conf_sum %% 3 == 0))
  expect_true(all(abs(output_df$conf_sum - output_df$raw_sum) < 3))
})

test_that("rounding can be stable", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df_r1 = apply_random_rounding(input_df, "count")
  output_df_r2 = apply_random_rounding(input_df, "count")
  output_df_s1 = apply_random_rounding(input_df, "count", stable_across_cols = "label1")
  output_df_s2 = apply_random_rounding(input_df, "count", stable_across_cols = "label1")
  output_df_s3 = apply_random_rounding(input_df, "count", stable_across_cols = c("label1", "label2"))
  output_df_s4 = apply_random_rounding(input_df, "count", stable_across_cols = c("label1", "label2"))
  # assert
  expect_false(compare_all_equal(output_df_r1, output_df_r2, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s1, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s2, "raw_count"))
  expect_true(compare_all_equal(output_df_s1, output_df_s2, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s3, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s4, "raw_count"))
  expect_false(compare_all_equal(output_df_s1, output_df_s3, "raw_count"))
  expect_false(compare_all_equal(output_df_s1, output_df_s4, "raw_count"))
  expect_true(compare_all_equal(output_df_s3, output_df_s4, "raw_count"))
})

test_that("stable rounding is independent of column order", {
  # arrange
  input_df1 = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    count = 2 + 1:100
  )
  input_df2 = data.frame(
    label1 = c(rep("z",50), rep("y",50)),
    label2 = rep(c("a","b"), 50),
    count = 2 + 1:100
  )
  # act
  output1 = apply_random_rounding(input_df1, "count", stable_across_cols = c("label1", "label2"))
  output2 = apply_random_rounding(input_df1, "count", stable_across_cols = c("label2", "label1"))
  output3 = apply_random_rounding(input_df2, "count", stable_across_cols = c("label1", "label2"))
  output4 = apply_random_rounding(input_df2, "count", stable_across_cols = c("label2", "label1"))
  # force consistent colnames
  output3 = dplyr::select(output3, label1 = label2, label2 = label1, raw_count, conf_count)
  output4 = dplyr::select(output4, label1 = label2, label2 = label1, raw_count, conf_count)
  
  # assert
  expect_true(compare_all_equal(output1, output2, "raw_count"))
  expect_true(compare_all_equal(output3, output4, "raw_count"))
  expect_true(compare_all_equal(output1, output3, "raw_count"))
  expect_true(compare_all_equal(output2, output4, "raw_count"))
})

test_that("input checks stop execution", {
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  
  expect_error(apply_random_rounding("input_df", "count", BASE = 3, stable_across_cols = NULL), "data\\.frame")
  expect_error(apply_random_rounding(input_df, "not col", BASE = 3, stable_across_cols = NULL), "column")
  expect_error(apply_random_rounding(input_df, "count", BASE = "3", stable_across_cols = NULL), "numeric")
  expect_error(apply_random_rounding(input_df, "count", BASE = 3, stable_across_cols = "not col"), "colnames")
  expect_error(apply_random_rounding(input_df, "count", BASE = 3, stable_across_cols = "count"), "RR_columns")
})

## apply_graduated_random_rounding(df, GRR_columns, stable_across_cols = NULL) ----

test_that("new columns created", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df1 = apply_graduated_random_rounding(input_df, "count")
  output_df2 = apply_graduated_random_rounding(input_df, "distinct")
  output_df3 = apply_graduated_random_rounding(input_df, c("distinct", "count"))
  # assert
  expect_true(all(c("raw_count", "conf_count") %in% colnames(output_df1)))
  expect_true(all(c("raw_count", "conf_count") %in% colnames(output_df3)))
  expect_true(all(c("raw_distinct", "conf_distinct") %in% colnames(output_df2)))
  expect_true(all(c("raw_distinct", "conf_distinct") %in% colnames(output_df3)))
})

test_that("graduated random rounding applied", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    count0_18 = sample(1:18, 100, replace = TRUE),
    count19 = rep(19, 100),
    count20_99 = sample(20:99, 100, replace = TRUE),
    count100_999 = sample(100:999, 100, replace = TRUE),
    count1000 = sample(1000:3000, 100, replace = TRUE)
  )
  # act
  output_df0_18 = apply_graduated_random_rounding(input_df, "count0_18")
  output_df19 = apply_graduated_random_rounding(input_df, "count19")
  output_df20_99 = apply_graduated_random_rounding(input_df, "count20_99")
  output_df100_999 = apply_graduated_random_rounding(input_df, "count100_999")
  output_df1000 = apply_graduated_random_rounding(input_df, "count1000")
  # assert
  expect_true(all(output_df0_18$conf_count %% 3 == 0))
  expect_true(all(output_df19$conf_count %% 2 == 0))
  expect_true(all(output_df20_99$conf_count %% 5 == 0))
  expect_true(all(output_df100_999$conf_count %% 10 == 0))
  expect_true(all(output_df1000$conf_count %% 100 == 0))
  expect_true(all(abs(output_df0_18$conf_count - output_df0_18$raw_count) < 3))
  expect_true(all(abs(output_df19$conf_count - output_df19$raw_count) < 2))
  expect_true(all(abs(output_df20_99$conf_count - output_df20_99$raw_count) < 5))
  expect_true(all(abs(output_df100_999$conf_count - output_df100_999$raw_count) < 10))
  expect_true(all(abs(output_df1000$conf_count - output_df1000$raw_count) < 100))
})

test_that("graduated rounding can be stable", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    count = 2 + 1:100
  )
  # act
  output_df_r1 = apply_graduated_random_rounding(input_df, "count")
  output_df_r2 = apply_graduated_random_rounding(input_df, "count")
  output_df_s1 = apply_graduated_random_rounding(input_df, "count", stable_across_cols = "label1")
  output_df_s2 = apply_graduated_random_rounding(input_df, "count", stable_across_cols = "label1")
  output_df_s3 = apply_graduated_random_rounding(input_df, "count", stable_across_cols = c("label1", "label2"))
  output_df_s4 = apply_graduated_random_rounding(input_df, "count", stable_across_cols = c("label1", "label2"))
  # assert
  expect_false(compare_all_equal(output_df_r1, output_df_r2, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s1, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s2, "raw_count"))
  expect_true(compare_all_equal(output_df_s1, output_df_s2, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s3, "raw_count"))
  expect_false(compare_all_equal(output_df_r1, output_df_s4, "raw_count"))
  expect_false(compare_all_equal(output_df_s1, output_df_s3, "raw_count"))
  expect_false(compare_all_equal(output_df_s1, output_df_s4, "raw_count"))
  expect_true(compare_all_equal(output_df_s3, output_df_s4, "raw_count"))
})

test_that("stable graduated rounding is independent of column order", {
  # arrange
  input_df1 = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    count = 2 + 1:100
  )
  input_df2 = data.frame(
    label1 = c(rep("z",50), rep("y",50)),
    label2 = rep(c("a","b"), 50),
    count = 2 + 1:100
  )
  # act
  output1 = apply_graduated_random_rounding(input_df1, "count", stable_across_cols = c("label1", "label2"))
  output2 = apply_graduated_random_rounding(input_df1, "count", stable_across_cols = c("label2", "label1"))
  output3 = apply_graduated_random_rounding(input_df2, "count", stable_across_cols = c("label1", "label2"))
  output4 = apply_graduated_random_rounding(input_df2, "count", stable_across_cols = c("label2", "label1"))
  # force consistent colnames
  output3 = dplyr::select(output3, label1 = label2, label2 = label1, raw_count, conf_count)
  output4 = dplyr::select(output4, label1 = label2, label2 = label1, raw_count, conf_count)
  
  # assert
  expect_true(compare_all_equal(output1, output2, "raw_count"))
  expect_true(compare_all_equal(output3, output4, "raw_count"))
  expect_true(compare_all_equal(output1, output3, "raw_count"))
  expect_true(compare_all_equal(output2, output4, "raw_count"))
})


test_that("input checks stop execution", {
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  
  expect_error(apply_graduated_random_rounding("input_df", "count", stable_across_cols = NULL), "data\\.frame")
  expect_error(apply_graduated_random_rounding(input_df, "not col", stable_across_cols = NULL), "colnames")
  expect_error(apply_graduated_random_rounding(input_df, "count", stable_across_cols = "not col"), "colnames")
  expect_error(apply_graduated_random_rounding(input_df, "count", stable_across_cols = "count"), "GRR_columns")
})

## apply_small_count_suppression(df, suppress_cols, threshold, count_cols = suppress_cols) ----

test_that("suppression occurs", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = 10 + 1:10
  )
  expect_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = c(NA,NA,NA,6,7,8,9,10,11,12),
    sum = 10 + 1:10
  )
  # act
  output_df = apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 6)
  # assert
  expect_true(compare_all_equal(output_df, expect_df, "sum"))
})

test_that("suppressing different columns work", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = 10 + 1:10
  )
  expect_df1 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = c(NA,NA,NA,6,7,8,9,10,11,12),
    sum = 10 + 1:10
  )
  expect_df2 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = c(NA,NA,NA,NA,NA,6,7,8,9,10),
    count = 2 + 1:10,
    sum = 10 + 1:10
  )
  # act
  output_df1 = apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 6)
  output_df2 = apply_small_count_suppression(input_df, suppress_cols = "distinct", threshold = 6)
  # assert
  expect_true(compare_all_equal(output_df1, expect_df1, "sum"))
  expect_true(compare_all_equal(output_df2, expect_df2, "sum"))
})

test_that("suppressing multi columns work", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = 10 + 1:10
  )
  expect_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = c(NA,NA,NA,NA,NA,6,7,8,9,10),
    count = c(NA,NA,NA,NA,NA,8,9,10,11,12),
    sum = 10 + 1:10
  )
  
  # act
  output_df = apply_small_count_suppression(input_df, suppress_cols = c("distinct", "count"), threshold = 6)
  # assert
  expect_true(compare_all_equal(output_df, expect_df, "sum"))
})

test_that("suppressing non-count columns works", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = 10 + 1:10
  )
  expect_df1 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = c(NA,NA,NA,14,15,16,17,18,19,20)
  )
  expect_df2 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = c(NA,NA,NA,NA,NA,16,17,18,19,20)
  )
  
  # act
  output_df1 = apply_small_count_suppression(input_df, suppress_cols = "sum", threshold = 6, count_cols = "count")
  output_df2 = apply_small_count_suppression(input_df, suppress_cols = "sum" , threshold = 6, count_cols = c("distinct", "count"))
  # assert
  expect_true(compare_all_equal(output_df1, expect_df1, "distinct"))
  expect_true(compare_all_equal(output_df2, expect_df2, "distinct"))
})

test_that("different thresholds perform", {
  # arrange
  input_df = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = 2 + 1:10,
    sum = 10 + 1:10
  )
  expect_df1 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = c(3,4,5,6,7,8,9,10,11,12),
    sum = 10 + 1:10
  )
  expect_df2 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = c(NA,NA,5,6,7,8,9,10,11,12),
    sum = 10 + 1:10
  )
  expect_df3 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = c(NA,NA,NA,NA,NA,8,9,10,11,12),
    sum = 10 + 1:10
  )
  expect_df4 = data.frame(
    label1 = rep(c("a","b"), 5),
    label2 = c(rep("z",5), rep("y",5)),
    distinct = 1:10,
    count = rep(NA_integer_, 10),
    sum = 10 + 1:10
  )
  
  # act
  output_df1 = apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 3)
  output_df2 = apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 5)
  output_df3 = apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 8)
  output_df4 = apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 90)
  # assert
  expect_true(compare_all_equal(output_df1, expect_df1, c("label1", "label2")))
  expect_true(compare_all_equal(output_df2, expect_df2, c("label1", "label2")))
  expect_true(compare_all_equal(output_df3, expect_df3, c("label1", "label2")))
  expect_true(compare_all_equal(output_df4, expect_df4, c("label1", "label2")))
})

test_that("input checks stop execution", {
  input_df = data.frame(
    label1 = rep(c("a","b"), 50),
    label2 = c(rep("z",50), rep("y",50)),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  
  expect_error(apply_small_count_suppression("input_df", suppress_cols = "count", threshold = 3), "data\\.frame")
  expect_error(apply_small_count_suppression(input_df, suppress_cols = "count", threshold = "3"), "numeric")
  expect_error(apply_small_count_suppression(input_df, suppress_cols = "not col", threshold = 3), "colname")
  expect_error(apply_small_count_suppression(input_df, suppress_cols = "count", threshold = 3, count_cols = "not col"), "colname")
})

## confidentialise_results(df,stable_RR = FALSE,sum_RR = FALSE,BASE = 3,COUNT_THRESHOLD = 6,SUM_THRESHOLD = 20) ----

test_that("confidentialisation random rounding occurs", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  input_compare = dplyr::rename(input_df, raw_distinct = distinct, raw_count = count, raw_sum = sum)
  output_compare = dplyr::select(output_df, -conf_distinct, -conf_count, -conf_sum)
  # assert
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df$conf_count) | output_df$conf_count %% 3 == 0))
  expect_true(all(is.na(output_df$conf_count) | abs(output_df$conf_count - output_df$raw_count) < 3))
  expect_true(all(is.na(output_df$conf_distinct) | output_df$conf_distinct %% 3 == 0))
  expect_true(all(is.na(output_df$conf_distinct) | abs(output_df$conf_distinct - output_df$raw_distinct) < 3))
})

test_that("confidentialisation censoring occurs", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  expect_na_count = input_df$count < 6 | input_df$distinct < 6
  expect_na_sum = input_df$count < 20 | input_df$distinct < 20
  # assert
  expect_true(all(is.na(output_df$conf_distinct[expect_na_count])))
  expect_true(all(is.na(output_df$conf_count[expect_na_count])))
  expect_true(all(is.na(output_df$conf_sum[expect_na_sum])))
  expect_true(all(!is.na(output_df$conf_distinct[!expect_na_count])))
  expect_true(all(!is.na(output_df$conf_count[!expect_na_count])))
  expect_true(all(!is.na(output_df$conf_sum[!expect_na_sum])))
})

test_that("confidentialisation all occurs", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  expect_na_count = input_df$count < 6 | input_df$distinct < 6
  expect_na_sum = input_df$count < 20 | input_df$distinct < 20
  input_compare = dplyr::rename(input_df, raw_distinct = distinct, raw_count = count, raw_sum = sum)
  output_compare = dplyr::select(output_df, -conf_distinct, -conf_count, -conf_sum)
  # assert
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df$conf_count) | output_df$conf_count %% 3 == 0))
  expect_true(all(is.na(output_df$conf_count) | abs(output_df$conf_count - output_df$raw_count) < 3))
  expect_true(all(is.na(output_df$conf_distinct) | output_df$conf_distinct %% 3 == 0))
  expect_true(all(is.na(output_df$conf_distinct) | abs(output_df$conf_distinct - output_df$raw_distinct) < 3))
  expect_true(all(is.na(output_df$conf_distinct[expect_na_count])))
  expect_true(all(is.na(output_df$conf_count[expect_na_count])))
  expect_true(all(is.na(output_df$conf_sum[expect_na_sum])))
  expect_true(all(!is.na(output_df$conf_distinct[!expect_na_count])))
  expect_true(all(!is.na(output_df$conf_count[!expect_na_count])))
  expect_true(all(!is.na(output_df$conf_sum[!expect_na_sum])))
})

test_that("threshold difference handled", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = rep(20, 100),
    count = rep(20, 100),
    sum = 10 + 1:100
  )
  # act - round up
  output_df = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  # assert
  expect_true(all(output_df$conf_distinct == 21))
  expect_true(all(output_df$conf_count == 21))
  expect_true(all(output_df$raw_sum == output_df$conf_sum))
  
  # act - round down
  input_df$distinct = 19
  input_df$count = 19
  output_df = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  # assert
  expect_true(all(output_df$conf_distinct == 18))
  expect_true(all(output_df$conf_count == 18))
  expect_true(all(is.na(output_df$conf_sum)))
})

test_that("options for random rounding respected", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = TRUE)
  input_compare = dplyr::rename(input_df, raw_distinct = distinct, raw_count = count, raw_sum = sum)
  output_compare = dplyr::select(output_df, -conf_distinct, -conf_count, -conf_sum)
  # assert
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df$conf_sum) | output_df$conf_sum %% 3 == 0))
  expect_true(all(is.na(output_df$conf_sum) | abs(output_df$conf_sum - output_df$raw_sum) < 3))
})

test_that("confidentialisation can be stable", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df_r1 = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  output_df_r2 = confidentialise_results(input_df, stable_RR = FALSE, sum_RR = FALSE)
  output_df_s1 = confidentialise_results(input_df, stable_RR = TRUE, sum_RR = FALSE)
  output_df_s2 = confidentialise_results(input_df, stable_RR = TRUE, sum_RR = FALSE)
  output_df_s3 = confidentialise_results(input_df, stable_RR = TRUE, sum_RR = TRUE)
  output_df_s4 = confidentialise_results(input_df, stable_RR = TRUE, sum_RR = TRUE)
  # assert
  expect_false(compare_all_equal(output_df_r1, output_df_r2, c("col01", "val01", "col02", "val02")))
  expect_false(compare_all_equal(output_df_r1, output_df_s1, c("col01", "val01", "col02", "val02")))
  expect_false(compare_all_equal(output_df_r1, output_df_s2, c("col01", "val01", "col02", "val02")))
  expect_true(compare_all_equal(output_df_s1, output_df_s2, c("col01", "val01", "col02", "val02")))
  expect_false(compare_all_equal(output_df_s1, output_df_s3, c("col01", "val01", "col02", "val02")))
  expect_true(compare_all_equal(output_df_s3, output_df_s4, c("col01", "val01", "col02", "val02")))
})

test_that("confidentialisation accepts incomplete columns", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  input_df010 = dplyr::select(input_df, -distinct, -sum)
  input_df100 = dplyr::select(input_df, -count, -sum)
  input_df011 = dplyr::select(input_df, -distinct)
  input_df101 = dplyr::select(input_df, -count)
  
  # act - count only
  output_df010 = confidentialise_results(input_df010, stable_RR = FALSE, sum_RR = TRUE)
  expect_na_count = input_df010$count < 6
  input_compare = dplyr::rename(input_df010, raw_count = count)
  output_compare = dplyr::select(output_df010, -conf_count)
  # assess
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df010$conf_count) | output_df010$conf_count %% 3 == 0))
  expect_true(all(is.na(output_df010$conf_count) | abs(output_df010$conf_count - output_df010$raw_count) < 3))
  expect_true(all(is.na(output_df010$conf_count[expect_na_count])))
  expect_true(all(!is.na(output_df010$conf_count[!expect_na_count])))
  
  # act - distinct only
  output_df100 = confidentialise_results(input_df100, stable_RR = FALSE, sum_RR = TRUE)
  expect_na_count = input_df100$distinct < 6
  input_compare = dplyr::rename(input_df100, raw_distinct = distinct)
  output_compare = dplyr::select(output_df100, -conf_distinct)
  # assess
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df100$conf_distinct) | output_df100$conf_distinct %% 3 == 0))
  expect_true(all(is.na(output_df100$conf_distinct) | abs(output_df100$conf_distinct - output_df100$raw_distinct) < 3))
  expect_true(all(is.na(output_df100$conf_distinct[expect_na_count])))
  expect_true(all(!is.na(output_df100$conf_distinct[!expect_na_count])))
  
  # act - count and sum
  output_df011 = confidentialise_results(input_df011, stable_RR = FALSE, sum_RR = TRUE)
  expect_na_count = output_df011$count < 6
  expect_na_sum = output_df011$count < 20
  input_compare = dplyr::rename(input_df011, raw_count = count, raw_sum = sum)
  output_compare = dplyr::select(output_df011, -conf_count, -conf_sum)
  # assess
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df011$conf_count) | output_df011$conf_count %% 3 == 0))
  expect_true(all(is.na(output_df011$conf_count) | abs(output_df011$conf_count - output_df011$raw_count) < 3))
  expect_true(all(is.na(output_df011$conf_count[expect_na_count])))
  expect_true(all(is.na(output_df011$conf_sum[expect_na_sum])))
  expect_true(all(!is.na(output_df011$conf_count[!expect_na_count])))
  expect_true(all(!is.na(output_df011$conf_sum[!expect_na_sum])))
  
  # act -distinct and sum
  output_df101 = confidentialise_results(input_df101, stable_RR = FALSE, sum_RR = TRUE)
  expect_na_count = input_df101$distinct < 6
  expect_na_sum = input_df101$distinct < 20
  input_compare = dplyr::rename(input_df101, raw_distinct = distinct, raw_sum = sum)
  output_compare = dplyr::select(output_df101, -conf_distinct, -conf_sum)
  # assess
  expect_true(compare_all_equal(input_compare, output_compare, c("col01", "val01", "col02", "val02")))
  expect_true(all(is.na(output_df101$conf_distinct) | output_df101$conf_distinct %% 3 == 0))
  expect_true(all(is.na(output_df101$conf_distinct) | abs(output_df101$conf_distinct - output_df101$raw_distinct) < 3))
  expect_true(all(is.na(output_df101$conf_distinct[expect_na_count])))
  expect_true(all(is.na(output_df101$conf_sum[expect_na_sum])))
  expect_true(all(!is.na(output_df101$conf_distinct[!expect_na_count])))
  expect_true(all(!is.na(output_df101$conf_sum[!expect_na_sum])))
})

test_that("variable parameters accepted", {
  # arrange
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  # act
  output_df1 = confidentialise_results(input_df, BASE = 2, COUNT_THRESHOLD = 10, SUM_THRESHOLD = 10)
  output_df2 = confidentialise_results(input_df, BASE = 7, COUNT_THRESHOLD = 12, SUM_THRESHOLD = 24)
  output_df3 = confidentialise_results(input_df, BASE = 12, COUNT_THRESHOLD = 3, SUM_THRESHOLD = 5)
  
  # assert
  expect_true(all(is.na(output_df1$conf_count) | output_df1$conf_count %% 2 == 0))
  expect_true(all(is.na(output_df1$conf_count) | abs(output_df1$conf_count - output_df1$raw_count) < 2))
  expect_true(all(is.na(output_df1$conf_count[input_df$count < 10])))
  expect_true(all(is.na(output_df1$conf_sum[input_df$count < 10])))
  expect_true(all(!is.na(output_df1$conf_count[!input_df$count < 10])))
  expect_true(all(!is.na(output_df1$conf_sum[!input_df$count < 10])))
  
  expect_true(all(is.na(output_df2$conf_count) | output_df2$conf_count %% 7 == 0))
  expect_true(all(is.na(output_df2$conf_count) | abs(output_df2$conf_count - output_df2$raw_count) < 7))
  expect_true(all(is.na(output_df2$conf_count[input_df$count < 12])))
  expect_true(all(is.na(output_df2$conf_sum[input_df$count < 24])))
  expect_true(all(!is.na(output_df2$conf_count[!input_df$count < 12])))
  expect_true(all(!is.na(output_df2$conf_sum[!input_df$count < 24])))
  
  expect_true(all(is.na(output_df3$conf_count) | output_df3$conf_count %% 12 == 0))
  expect_true(all(is.na(output_df3$conf_count) | abs(output_df3$conf_count - output_df3$raw_count) < 12))
  expect_true(all(is.na(output_df3$conf_count[input_df$count < 3])))
  expect_true(all(is.na(output_df3$conf_sum[input_df$count < 5])))
  expect_true(all(!is.na(output_df3$conf_count[!input_df$count < 3])))
  expect_true(all(!is.na(output_df3$conf_sum[!input_df$count < 5])))
})

test_that("input checks stop execution", {
  input_df = data.frame(
    col01 = rep("label1", 100),
    val01 = rep(c("a","b"), 50),
    col02 = rep("label2", 100),
    val02 = c(rep("z",50), rep("y",50)),
    summarised_var = rep("var", 100),
    distinct = 1:100,
    count = 2 + 1:100,
    sum = 10 + 1:100
  )
  poor_input_df = dplyr::select(input_df, -distinct, -count)
  # expect_error(summarise_and_label("input_df", "your_label", "rownum", TRUE, TRUE, TRUE, "none"), "data\\.frame")
  
  expect_error(confidentialise_results("input_df", stable_RR = FALSE, sum_RR = FALSE), "data\\.frame")
  expect_error(confidentialise_results(poor_input_df, stable_RR = FALSE, sum_RR = FALSE), "colnames")
  expect_error(confidentialise_results(input_df, stable_RR = "FALSE", sum_RR = FALSE), "logical")
  expect_error(confidentialise_results(input_df, stable_RR = "FALSE", sum_RR = FALSE), "logical")
})
