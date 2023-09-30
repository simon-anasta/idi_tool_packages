################################################################################
#' Description: Automated tests for utility functions.
#' Author: Simon Anastasiadis
#'
#' Notes:
#'
#' Issues:
#'
################################################################################

## run_time_inform_user(msg, context = NA, print_off = FALSE) ------------- ----

test_that("message output", {
  expect_output(run_time_inform_user("message text"), as.character(Sys.Date()))
  expect_output(run_time_inform_user("message text"), "message text")
})

test_that("context shows", {
  expect_output(run_time_inform_user("message text", "heading"), "MESSAGE")
  expect_output(run_time_inform_user("message text", "details"), " -- ")
  expect_output(run_time_inform_user("message text", "all"), " ---- ")
})

test_that("messages turned off", {
  msg <- "message text"
  expect_silent(run_time_inform_user(msg, print_off = TRUE))
  expect_silent(run_time_inform_user(msg, context = "heading", print_off = TRUE))
  expect_silent(run_time_inform_user(msg, context = "details", print_off = TRUE))
  expect_silent(run_time_inform_user(msg, context = "all", print_off = TRUE))
})

## "%not_in%"(x, y) ------------------------------------------------------- ----

test_that("multivariate LHS is correct", {
  not_in1 <- c(1, 2) %not_in% c(1, 2, 3)
  not_in2 <- c(8, 2) %not_in% c(1, 2, 3)
  in1 <- c(1, 2) %in% c(1, 2, 3)
  in2 <- c(8, 2) %in% c(1, 2, 3)
  
  expect_equal(not_in1, !in1)
  expect_equal(not_in2, !in2)
})

test_that("different calls work", {
  b1 <- c("a", "b", "c")
  b2 <- c("c", "d", "e")
  
  expect_equal(b1 %not_in% b2, `%not_in%`(b1, b2))
})

## no_special_characters(in_string) --------------------------------------- ----

test_that("special characters rejected", {
  expect_error(no_special_characters("foo (bar)"), "SPECIAL_CHARACTERS")
  expect_error(no_special_characters("foo'bar"), "SPECIAL_CHARACTERS")
  expect_error(no_special_characters("foo;bar"), "SPECIAL_CHARACTERS")
})

test_that("no special characters passed", {
  a1 <- no_special_characters("foo_bar")
  expect_identical(a1, NULL)
})

## is_delimited(string, delimiter) ---------------------------------------- ----

test_that("delimiters are checked", {
  expect_true(is_delimited("[string]", "[]"))
  expect_true(is_delimited('"string"', "\""))
  expect_true(is_delimited('"[string]"', "\""))
  expect_true(is_delimited('"string"', "\""))
  expect_true(is_delimited("\"string\"", "\""))
})

test_that("delimiters are not muddled", {
  expect_false(is_delimited("[string]", "\""))
  expect_false(is_delimited('"string"', "[]"))
  expect_false(is_delimited('"[string]"', "[]"))
  expect_false(is_delimited('"string"', "[]"))
  expect_false(is_delimited("\"string\"", "[]"))
})

test_that("non-sql delimiters work", {
  expect_true(is_delimited("astringa", "a"))
  expect_true(is_delimited("astringb", "ab"))
  expect_true(is_delimited("astringb", "ab"))
  expect_false(is_delimited(" string ", "ab"))
  expect_false(is_delimited("astring ", "ab"))
  expect_false(is_delimited(" stringb", "ab"))
})

## remove_delimiters(string, delimiter) ----------------------------------- ----

test_that("delimiters removed", {
  expect_equal(remove_delimiters("[text]", "[]"), "text")
  expect_equal(remove_delimiters("[odd][text]", "[]"), "odd][text")
  expect_equal(remove_delimiters("[text]", "\""), "[text]")
  expect_equal(remove_delimiters("\"text\"", "[]"), "\"text\"")
  expect_equal(remove_delimiters("\"text\"", "\""), "text")
  expect_equal(remove_delimiters("text", "[]"), "text")
  expect_equal(remove_delimiters("text", "t"), "ex")
  expect_equal(remove_delimiters("[text", "[]"), "text")
  expect_equal(remove_delimiters("text]", "[]"), "text")
})

## add_delimiters(string, delimiter) -------------------------------------- ----

test_that("delimiters added", {
  expect_equal(add_delimiters("text", "[]"), "[text]")
  expect_equal(add_delimiters("[text", "[]"), "[text]")
  expect_equal(add_delimiters("text]", "[]"), "[text]")
  expect_equal(add_delimiters("[text]", "[]"), "[text]")
  
  expect_equal(add_delimiters("[odd][text]", "[]"), "[odd][text]")
  expect_equal(add_delimiters("[text]", "\""), "\"[text]\"")
  expect_equal(add_delimiters("\"text\"", "[]"), "[\"text\"]")
})

## has_internal_delimiters(string, delimiter) ----------------------------- ----

test_that("regular delimiter cases pass", {
  expect_false(has_internal_delimiters("[string]", "[]"))
  expect_false(has_internal_delimiters("\"string\"", "\""))
  expect_false(has_internal_delimiters('"string"', "\""))
})

test_that("internal delimiters detected", {
  expect_true(has_internal_delimiters("[str].[ing]", "[]"))
  expect_true(has_internal_delimiters("[str]ing]", "[]"))
  expect_true(has_internal_delimiters("\"str\".\"ing\"", "\""))
  expect_true(has_internal_delimiters("\"str\"ing\"", "\""))
  expect_true(has_internal_delimiters("\"str'ing\"", "\""))
})
