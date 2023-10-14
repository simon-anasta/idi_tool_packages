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
#' 2021-09-08 SA v0
#' #############################################################################

#' Testing the following functions that support summarising and confidentialising
#'
#' randomly_round_vector(input_vector, base = 3, seeds = NULL)
#' 
context("summary confidential - support functions")

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
