################################################################################
# Description: Automated tests for checking confidentiality
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

## check_rounding_to_base_array(input_array, base = 3, na.rm = TRUE) ------ ----

test_that("rounding vector passes & fails when expected", {
  # arrange
  vec3 = (1:14) * 3
  vec4 = (1:14) * 4
  vec17 = (27:82) * 17
  vec3na = c(vec3, NA)
  vec4na = c(vec4, NA)
  vec17na = c(vec17, NA)
  
  # act & assert
  expect_true(check_rounding_to_base_array(vec3, base = 3, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec4, base = 4, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec4, base = 2, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = FALSE))
  
  expect_true(check_rounding_to_base_array(vec3, base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4, base = 4, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4, base = 2, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = TRUE))
  
  expect_true(check_rounding_to_base_array(vec3na, base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4na, base = 4, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4na, base = 2, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = TRUE))
  
  # act & assert
  expect_false(check_rounding_to_base_array(vec3na, base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec4na, base = 4, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec4na, base = 2, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec17na, base = 17, na.rm = FALSE))
  
  expect_false(check_rounding_to_base_array(1:50, base = 2, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec3, base = 6, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec3na, base = 4, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec4na, base = 3, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec17, base = 7, na.rm = TRUE))
})

test_that("rounding vector errors when expected", {
  
  expect_error(check_rounding_to_base_array(1:50, base = "3", na.rm = TRUE), "numeric")
  expect_error(check_rounding_to_base_array(1:50, base = 3, na.rm = "TRUE"), "logical")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = TRUE), "length")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = FALSE), "length")
  expect_error(check_rounding_to_base_array(c(1,2,3,NA,"NA"), base = 3, na.rm = TRUE), "numeric")
  
  expect_warning(check_rounding_to_base_array(c(NA, NA, NA), base = 3, na.rm = FALSE), "NA")
})

## check_rounding_to_base_df(df, column, base = 3, na.rm = TRUE) ---------- ----

test_that("rounding df passes & failed when expected", {
  # arrange
  input_df = data.frame(
    rr3 = 3 * 1:10,
    rr6 = 6 * 21:30,
    rrx = 41:50,
    rr3na = c(NA, 3 * 1:9),
    rr6na = c(6 * 61:69, NA)
  )
  
  # act & assert
  expect_true(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = TRUE))
  
  expect_false(check_rounding_to_base_df(input_df, "rr3na", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rr6na", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rr6na", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr3na", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6na", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6na", base = 6, na.rm = TRUE))
  
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 2, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 5, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 2, na.rm = TRUE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 3, na.rm = TRUE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 5, na.rm = TRUE))
})

test_that("rounding df errors when expected", {
  input_df = data.frame(rr3 = 3 * 1:5, rrx = 6:10, rr3na = c(3,6,3,9,NA), nas = c(NA, NA, NA, NA, NA))
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  expect_error(check_rounding_to_base_df("input_df", "rr3", base = 3, na.rm = FALSE), "data.frame")
  expect_error(check_rounding_to_base_df(remote_df, "rr3", base = 3, na.rm = FALSE), "sql")
  expect_error(check_rounding_to_base_df(input_df, 2, base = 3, na.rm = FALSE), "character")
  expect_error(check_rounding_to_base_df(input_df, "rr13", base = 3, na.rm = FALSE), "colname")
  
  # errors from passing column to vector
  expect_error(check_rounding_to_base_df(input_df, "rr3", base = "3", na.rm = TRUE), "numeric")
  expect_error(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = "TRUE"), "logical")
  
  expect_warning(check_rounding_to_base_df(input_df, "nas", base = 3, na.rm = TRUE), "NA")
})

## check_graduated_rounding_df(df, column, na.rm = TRUE) ------------------ ----

test_that("graduated rounding passes & failed when expected", {
  # arrange
  input_df = data.frame(
    grr = c(1:6*3, 18, 20, 4:20*5, 10:100*10, 10:20*100),
    gx1 = c(1:6*2, 18, 20, 4:20*5, 10:100*10, 10:20*100),
    gx2 = c(1:6*3, 19, 20, 4:20*5, 10:100*10, 10:20*100),
    gx3 = c(1:6*3, 18, 20, 4:20*5+1, 10:100*10, 10:20*100),
    gx4 = c(1:6*3, 18, 20, 4:20*5, 10:100*10+1, 10:20*100),
    gx5 = c(1:6*3, 18, 20, 4:20*5, 10:100*10, 10:20*100+1)
  )
  na_set = sample(1:nrow(input_df), 20)
  input_df$grrNA = input_df$grr
  input_df$grrNA[na_set] = NA

  # act & assert
  expect_true(check_graduated_rounding_df(input_df, "grr", na.rm = FALSE))
  expect_true(check_graduated_rounding_df(input_df, "grr", na.rm = TRUE))
  
  expect_false(check_graduated_rounding_df(input_df, "grrNA", na.rm = FALSE))
  expect_true(check_graduated_rounding_df(input_df, "grrNA", na.rm = TRUE))
  
  expect_false(check_graduated_rounding_df(input_df, "gx1", na.rm = FALSE))
  expect_false(check_graduated_rounding_df(input_df, "gx2", na.rm = FALSE))
  expect_false(check_graduated_rounding_df(input_df, "gx3", na.rm = FALSE))
  expect_false(check_graduated_rounding_df(input_df, "gx4", na.rm = TRUE))
  expect_false(check_graduated_rounding_df(input_df, "gx5", na.rm = TRUE))
})

test_that("graduated rounding errors when expected", {
  input_df = data.frame(
    grr = c(1:6*3, 18, 20, 4:20*5, 10:100*10, 10:20*100),
    gx1 = c(1:6*2, 18, 20, 4:20*5, 10:100*10, 10:20*100),
    gx2 = c(1:6*3, 19, 20, 4:20*5, 10:100*10, 10:20*100),
    gx3 = c(1:6*3, 18, 20, 4:20*5+1, 10:100*10, 10:20*100),
    gx4 = c(1:6*3, 18, 20, 4:20*5, 10:100*10+1, 10:20*100),
    gx5 = c(1:6*3, 18, 20, 4:20*5, 10:100*10, 10:20*100+1)
  )
  input_df$nas = NA
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  expect_error(check_graduated_rounding_df("input_df", "grr", na.rm = FALSE), "data.frame")
  expect_error(check_graduated_rounding_df(remote_df, "grr", na.rm = FALSE), "sql")
  expect_error(check_graduated_rounding_df(input_df, 2, na.rm = FALSE), "character")
  expect_error(check_graduated_rounding_df(input_df, "grrXX", na.rm = FALSE), "colname")
  
  # errors from passing column to vector
  expect_error(check_graduated_rounding_df(input_df, "grr", na.rm = "TRUE"), "logical")
  
  expect_warning(check_graduated_rounding_df(input_df, "nas", na.rm = TRUE), "NA")
})

## check_random_rounding(df, raw_col = NA, conf_col, base = 3) ------------ ----

test_that("random rounding checks pass & fail when expected", {
  # arrange
  input_df = data.frame(
    raw = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
    round_down = c(0,0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21),
    round_up = c(3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24,24),
    round_closest = c(0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21),
    round_furthest = c(3,0,3,6,3,6,9,6,9,12,9,12,15,12,15,18,15,18,21,18,21,24),
    round_random1 = c(3,3,3,3,6,6,6,9,9,12,12,12,12,15,15,18,18,18,21,18,21,21),
    round_random2 = c(0,0,3,3,3,6,6,6,9,12,12,12,12,12,15,15,18,18,18,21,21,21),
    round_random3 = c(3,3,3,3,6,6,9,9,9,9,9,12,12,15,15,15,18,18,18,18,21,21)
  )
  
  # act & assert
  expect_true(check_random_rounding(input_df, "raw", "round_random1", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_random2", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_random3", base = 3))
  expect_false(check_random_rounding(input_df, "round_random1", "raw", base = 3))
  
  defaultW = getOption("warn") 
  options(warn = -1) 
  
  expect_true(check_random_rounding(input_df, "raw", "round_down", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_up", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_closest", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_furthest", base = 3))
  
  expect_false(check_random_rounding(input_df, "raw", "raw", base = 3))
  
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_random1", base = 3))
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_random2", base = 3))
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_random3", base = 3))
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_down", base = 3))
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_up", base = 3))
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_closest", base = 3))
  expect_true(check_random_rounding(input_df, "raw", conf_col = "round_furthest", base = 3))
  
  expect_false(check_random_rounding(input_df, "raw", conf_col = "raw", base = 3))
  
  options(warn = defaultW)
})

test_that("random rounding checks warn & error when expected", {
  # arrange
  input_df = data.frame(
    raw = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
    round_down = c(0,0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21),
    round_up = c(3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24),
    round_closest = c(0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21),
    round_furthest = c(3,0,3,6,3,6,9,6,9,12,9,12,15,12,15,18,15,18,21,18,21,24),
    round_random1 = c(3,3,3,3,6,6,6,9,9,12,12,12,12,15,15,18,18,18,21,18,21,21),
    round_random2 = c(0,0,3,3,3,6,6,6,9,12,12,12,12,12,15,15,18,18,18,21,21,21),
    round_random3 = c(3,3,3,3,6,6,9,9,9,9,9,12,12,15,15,15,18,18,18,18,21,21)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  # act & assert
  expect_error(check_random_rounding("input_df", "raw", "round_random1", base = 3), "data.frame")
  expect_error(check_random_rounding(remote_df, "raw", "round_random1", base = 3), "sql")
  expect_error(check_random_rounding(input_df, 2, "round_random1", base = 3), "character")
  expect_error(check_random_rounding(input_df, "raw_x", "round_random1", base = 3), "colname")
  expect_error(check_random_rounding(input_df, "raw", NA, base = 3), "character")
  expect_error(check_random_rounding(input_df, "raw", "round_random1_x", base = 3), "colname")
  
  suppressWarnings(expect_warning(check_random_rounding(input_df, "round_down", "round_up", base = 3), "difference exceeds"))
  suppressWarnings(expect_warning(check_random_rounding(input_df, "raw", "round_up", base = 3), "rounded up"))
  suppressWarnings(expect_warning(check_random_rounding(input_df, "raw", "round_down", base = 3), "rounded down"))
  
  expect_warning(check_random_rounding(input_df, "raw", "round_closest", base = 3), "not random")
  expect_warning(check_random_rounding(input_df, "raw", "round_furthest", base = 3), "not random")
})

## check_small_count_suppression(df, suppressed_col, threshold, count_col = suppressed_col) ----

test_that("small count suppression pass & fail when expected", {
  # arrange
  input_df = data.frame(
    raw_count = 1:30,
    conf_count = c(rep_len(NA, 5), 6:30),
    conf_sum = c(rep_len(NA, 19), 20:30 * 100)
  )
  
  # act & assert
  expect_true(check_small_count_suppression(input_df, "conf_count", 6, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_count", 6))
  expect_true(check_small_count_suppression(input_df, "conf_count", 4, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_count", 4))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 20, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 20, count_col = "conf_count"))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 15, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 15, count_col = "conf_count"))
  
  expect_false(check_small_count_suppression(input_df, "conf_count", 8, count_col = "raw_count"))
  expect_false(check_small_count_suppression(input_df, "conf_count", 8))
  expect_false(check_small_count_suppression(input_df, "conf_sum", 22, count_col = "raw_count"))
  expect_false(check_small_count_suppression(input_df, "conf_sum", 22, count_col = "conf_count"))
  expect_false(check_small_count_suppression(input_df, "raw_count", 6, count_col = "conf_count"))
})

test_that("small count suppression error when expected", {
  # arrange
  input_df = data.frame(
    raw_count = 1:30,
    conf_count = c(rep_len(NA, 5), 6:30),
    conf_sum = c(rep_len(NA, 19), 20:30 * 100)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  
  check_small_count_suppression(input_df, "conf_count", 6, count_col = "raw_count")
  
  expect_error(check_small_count_suppression("input_df", "conf_count", 6, count_col = "raw_count"), "data.frame")
  expect_error(check_small_count_suppression(remote_df, "conf_count", 6, count_col = "raw_count"), "sql")
  expect_error(check_small_count_suppression(input_df, NA, 6, count_col = "raw_count"), "character")
  expect_error(check_small_count_suppression(input_df, "conf_count", 6, count_col = 4), "character")
  expect_error(check_small_count_suppression(input_df, "conf_count_x", 6, count_col = "raw_count"), "colname")
  expect_error(check_small_count_suppression(input_df, "conf_count", 6, count_col = "raw_count_x"), "colname")
  expect_error(check_small_count_suppression(input_df, "conf_count", "6", count_col = "raw_count"), "numeric")
})

## check_absence_of_zero_counts(df, conf_count_col, print_on_fail = FALSE)  ----

test_that("absence of zeroes pass & fail when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  
  # no NAs
  expect_true(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count"))
  
  for(ii in 1:nrow(input_df)){
    tmp_df = dplyr::filter(input_df, dplyr::row_number() != ii)
    expect_true(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
  }
  
  # with NAs
  input_df$conf_count[1:2] = NA
  expect_true(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count"))
  
  for(ii in 1:nrow(input_df)){
    tmp_df = dplyr::filter(input_df, dplyr::row_number() != ii)
    if(ii %in% 1:6){
      expect_false(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
    } else {
      expect_true(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
    }
  }
  
  # with NAs 2
  input_df$conf_count[15:16] = NA
  expect_true(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count"))
  
  for(ii in 1:nrow(input_df)){
    tmp_df = dplyr::filter(input_df, dplyr::row_number() != ii)
    expect_false(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
  }
})

test_that("absence of zeroes errors when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  alt_df = dplyr::rename(input_df, wrong_col_name = col01)
  
  # act & assert
  expect_error(check_absence_of_zero_counts("input_df", conf_count_col = "conf_count"), "data.frame")
  expect_error(check_absence_of_zero_counts(remote_df, conf_count_col = "conf_count"), "sql")
  expect_error(check_absence_of_zero_counts(alt_df, conf_count_col = "conf_count"), "long_thin")
  expect_error(check_absence_of_zero_counts(input_df, conf_count_col = 4), "character")
  expect_error(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count_x"), "colname")
  expect_error(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count", print_on_fail = "TRUE"), "logical")
})

## expand_to_include_zero_counts(df) -------------------------------------- ----

test_that("expand to include zeroes creates expected output", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  
  # complete dataset is unchanged
  output_df = expand_to_include_zero_counts(input_df)
  expect_true(compare_all_equal(input_df, output_df, c("col01", "val01", "col02", "val02", "summarised_var")))
  
  # missing rows added back in
  for(ii in 1:nrow(input_df)){
    expected_output = input_df
    expected_output$conf_count[ii] = NA
    
    tmp_input = dplyr::filter(expected_output, !is.na(conf_count))
    
    tmp_output = expand_to_include_zero_counts(tmp_input)
    expect_true(compare_all_equal(expected_output, tmp_output, c("col01", "val01", "col02", "val02", "summarised_var")))
  }
  
  # missing pairs of rows
  for(ii in 1:2){
    for(jj in 4:nrow(input_df)){
      expected_output = input_df
      expected_output$conf_count[ii] = NA
      expected_output$conf_count[jj] = NA
      
      tmp_input = dplyr::filter(expected_output, !is.na(conf_count))
      
      tmp_output = expand_to_include_zero_counts(tmp_input)
      
      expect_true(
        # either we match the expected output
        compare_all_equal(expected_output, tmp_output, c("col01", "val01", "col02", "val02", "summarised_var"))
        # or input = output because we do not need to add any rows
        | compare_all_equal(tmp_input, tmp_output, c("col01", "val01", "col02", "val02", "summarised_var"))
      )
    }
  }
})

test_that("expand to include zeroes errors when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  alt_df = dplyr::rename(input_df, wrong_col_name = col01)
  
  # act & assert
  expect_error(expand_to_include_zero_counts("input_df"), "data.frame")
  expect_error(expand_to_include_zero_counts(remote_df), "sql")
  expect_error(expand_to_include_zero_counts(alt_df), "long_thin")
})

## check_confidentialised_results(df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20) ----

test_that("checking conf results returns correct messages", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    raw_distinct  = c( 3, 4, 5, 5, 7, 8, 9,10,11,13,14,45,46,47,48,49,50,51),
    conf_distinct = c(NA,NA,NA,NA, 9, 9, 9, 9,12,15,12,45,45,48,48,48,51,51),
    raw_count     = c( 3, 4, 5, 6, 7, 8, 9,10,11,22,23,54,55,56,57,58,59,60),
    conf_count    = c(NA,NA,NA, 6, 9, 6, 9, 9,12,21,24,54,57,54,57,57,60,60),
    raw_sum       = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30),
    conf_sum      = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,30,72,33,75,63,90,30)
  )
  
  # act & arrange - all pass
  msg = check_confidentialised_results(input_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_distinct\\s+checked for RR[0-9]+ : PASS")
  expect_output(print(msg), "conf_distinct\\s+suppressed if raw < [0-9]+ : PASS")
  expect_output(print(msg), "conf_count\\s+checked for RR[0-9]+ : PASS")
  expect_output(print(msg), "conf_count\\s+suppressed if raw < [0-9]+ : PASS")
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < [0-9]+ : PASS")
  expect_output(print(msg), "all\\s+absence of zero counts : PASS")
  
  # act & arrange - pass conf_sum with only one check column
  tmp_df = input_df
  tmp_df$raw_count = NULL
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < [0-9]+ : PASS")
  
  tmp_df = input_df
  tmp_df$raw_distinct = NULL
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < [0-9]+ : PASS")
  
  # suppress warnings as warning are tested for elsewhere
  defaultW = getOption("warn") 
  options(warn = -1) 
  
  # act & arrange - fail distinct
  tmp_df = input_df
  tmp_df$conf_distinct = tmp_df$raw_distinct
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_distinct\\s+checked for RR[0-9]+ : FAIL")
  expect_output(print(msg), "conf_distinct\\s+suppressed if raw < [0-9]+ : FAIL")
  
  # act & arrange - skip distinct
  tmp_df = input_df
  tmp_df$raw_distinct = NULL
  tmp_df$conf_distinct = NULL
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_distinct\\s+checked for RR[0-9]+ : SKIP")
  expect_output(print(msg), "conf_distinct\\s+suppressed if raw < [0-9]+ : SKIP")
  
  # act & arrange - fail count
  tmp_df = input_df
  tmp_df$conf_count = tmp_df$raw_count
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_count\\s+checked for RR[0-9]+ : FAIL")
  expect_output(print(msg), "conf_count\\s+suppressed if raw < [0-9]+ : FAIL")
  
  # act & arrange - skip count
  tmp_df = input_df
  tmp_df$conf_count = NULL
  tmp_df$raw_count = NULL
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_count\\s+checked for RR[0-9]+ : SKIP")
  expect_output(print(msg), "conf_count\\s+suppressed if raw < [0-9]+ : SKIP")
  
  # act & arrange - fail sum
  tmp_df = input_df
  tmp_df$conf_sum = tmp_df$raw_sum
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < [0-9]+ : FAIL")
  
  # act & arrange - skip sum
  tmp_df = input_df
  tmp_df$conf_sum = NULL
  tmp_df$raw_sum = NULL
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < [0-9]+ : SKIP")
  
  # act & arrange - fail zeros
  tmp_df = input_df[2:13,]
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "all\\s+absence of zero counts : FAIL")
  
  # act & arrange - skip zeros
  tmp_df = input_df
  tmp_df$conf_count = NULL
  msg = check_confidentialised_results(tmp_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "all\\s+absence of zero counts : SKIP")
  
  # act & arrange - parameters change
  msg = check_confidentialised_results(input_df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
  expect_output(print(msg), "RR3")
  expect_output(print(msg), "conf_count\\s+suppressed if raw < 6")
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < 20")
  
  msg = check_confidentialised_results(input_df, BASE = 2, COUNT_THRESHOLD = 8, SUM_THRESHOLD = 76)
  expect_output(print(msg), "RR2")
  expect_output(print(msg), "conf_count\\s+suppressed if raw < 8")
  expect_output(print(msg), "conf_sum\\s+suppressed if raw < 76")
  
  # reactive warnings
  options(warn = defaultW)
})

test_that("checking conf results errors when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    raw_distinct  = c( 3, 4, 5, 5, 7, 8, 9,10,11,13,14,45,46,47,48,49,50,51),
    conf_distinct = c(NA,NA,NA,NA, 9, 9, 9, 9,12,15,12,45,45,48,48,48,51,51),
    raw_count     = c( 3, 4, 5, 6, 7, 8, 9,10,11,22,23,54,55,56,57,58,59,60),
    conf_count    = c(NA,NA,NA, 6, 9, 6, 9, 9,12,21,24,54,57,54,57,57,60,60),
    raw_sum       = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30),
    conf_sum      = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,30,72,33,75,63,90,30)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  alt_df = dplyr::rename(input_df, wrong_col_name = col01)
  
  # act & assert
  expect_error(check_confidentialised_results("input_df"), "data.frame")
  expect_error(check_confidentialised_results(remote_df), "sql")
  expect_error(check_confidentialised_results(alt_df), "long_thin")
  
  expect_error(check_confidentialised_results(input_df, BASE = "3"), "numeric")
  expect_error(check_confidentialised_results(input_df, COUNT_THRESHOLD = "6"), "numeric")
  expect_error(check_confidentialised_results(input_df, SUM_THRESHOLD = "20"), "numeric")
})
