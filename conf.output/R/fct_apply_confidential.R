################################################################################
# Description: Confidentialisation functionality
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Specific long-thin data format used and output by these functions.
# - Designed to work with both local/in-memory R data.frames and remote/SQL/
#   dbplyr-translated data sources.
#
# Issues:
#
################################################################################

## Filter to limited number of rows --------------------------------------- ----
#' Filter to a limited number of rows
#' 


## random rounding ------------------------------------------------------------
#'
#' Randomly rounds a numeric vector to the given base.
#' For example, if base is 3 (default) then:
#' - 3 & 6 will be left as is because they are already base 3;
#' - 4 & 7 will be rounded down with prob 2/3 and up with prob 1/3;
#' - 5 & 8 will be rounded down with prob 1/3 and up with prob /3.
#'
#' If seeds are provided, these will be used to round the corresponding
#' value in the input vector.
#' 
randomly_round_vector <- function(input_vector, base = 3, seeds = NULL){
  # check vector is numeric
  assert(is.numeric(input_vector), "input [input_vector] must be of type numeric")
  assert(is.numeric(base), "input [base] must be of type numeric")
  
  if(is.null(seeds)){
    probs = runif(length(input_vector))
  } else {
    assert(is.numeric(seeds), "input [seeds] must be of type numeric")
    assert(length(input_vector) == length(seeds), "if used, [seeds] must have same length as [input_vector]")
    probs = sapply(seeds, function(s){set.seed(s); runif(1)})
  }
  
  remainder_vector = input_vector %% base
  prob_round_down = (base - remainder_vector) / base
  ind_round_down = probs < prob_round_down
  
  output_vector = input_vector + base - remainder_vector - base * ind_round_down 
}



## apply random rounding ------------------------------------------------------
#'
#' Applied random rounding to specified columns.
#' Creates raw_ and conf_ columns so original values are preserved.
#' 
#' If stable_across_cols are provided, then these will be used to generate
#' seeds for random rounding to ensure that rounding is stable between 
#' repeated use of this function. Increases run time.
#' 
apply_random_rounding <- function(df, RR_columns, BASE = 3, stable_across_cols = NULL){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.numeric(BASE), "input [BASE] must be of type numeric")
  assert(all(RR_columns %in% colnames(df)), "[RR_columns] must be column names of [df]")
  # check stable_across_cols if provided
  if(!is.null(stable_across_cols)){
    assert(all(stable_across_cols %in% colnames(df)), "[stable_across_cols] must be column names of [df]")
    assert(!any(stable_across_cols %in% RR_columns), "[stable_across_cols] should not be found in [RR_columns]")
  }
  
  # loop through column types
  for(this_col in RR_columns){
    # value for this iteration
    raw_col = paste0("raw_", this_col)
    conf_col = paste0("conf_", this_col)
    
    # make raw_* column
    df = dplyr::rename(df, !!sym(raw_col) := !!sym(this_col))
    # make conf_* column
    if(is.null(stable_across_cols)){
      # round
      df = dplyr::mutate(df, !!sym(conf_col) := randomly_round_vector(!!sym(raw_col), base = BASE))
    } else {
      # make seeds
      sort_remove_nas = function(x){
        x = unname(x[!is.na(x)])
        return(paste(sort(x), collapse = " "))
      }
      concated = dplyr::select(df, dplyr::all_of(stable_across_cols)) %>%
        apply(MARGIN = 1, sort_remove_nas)
      
      df$tmp_concatenated = concated
      df$tmp_hashed = sapply(df$tmp_concatenated, digest::digest)  
      df = dplyr::mutate(df, tmp_seed = digest::digest2int(tmp_hashed))
      # round
      df = df %>%
        dplyr::mutate(!!sym(conf_col) := randomly_round_vector(!!sym(raw_col), base = BASE, seeds = tmp_seed)) %>%
        select(-tmp_concatenated, -tmp_seed, -tmp_hashed)
    }
    
  } # end for loop
  
  return(df)
}

## apply graduated random rounding --------------------------------------------
#'
#' Applied graduated random rounding (GRR) to specified columns.
#' Creates raw_ and conf_ columns so original values are preserved.
#' 
#' If stable_across_cols are provided, then these will be used to generate
#' seeds for random rounding to ensure that rounding is stable between 
#' repeated use of this function. Increases run time.
#' 
#' Thresholds for graduation are set by the Stats NZ microdata output guide.
#' Only apply one of GRR or RR3. Applyng both will cause errors.
#' 
apply_graduated_random_rounding <- function(df, GRR_columns, stable_across_cols = NULL){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(all(GRR_columns %in% colnames(df)), "[GRR_columns] must be column names of [df]")
  # check stable_across_cols if provided
  if(!is.null(stable_across_cols)){
    assert(all(stable_across_cols %in% colnames(df)), "[stable_across_cols] must be column names of [df]")
    assert(!any(stable_across_cols %in% GRR_columns), "[stable_across_cols] should not be found in [GRR_columns]")
  }
  
  # loop through column types
  for(this_col in GRR_columns){
    # value for this iteration
    raw_col = paste0("raw_", this_col)
    conf_col = paste0("conf_", this_col)
    
    # make raw_* column
    df = dplyr::rename(df, !!sym(raw_col) := !!sym(this_col))
    # make conf_* column
    if(is.null(stable_across_cols)){
      # round
      df = dplyr::mutate(df, !!sym(conf_col) := case_when(
        0 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 19 ~ 
          randomly_round_vector(!!sym(raw_col), base = 3),
        19 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 20 ~ 
          randomly_round_vector(!!sym(raw_col), base = 2),
        20 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 100 ~ 
          randomly_round_vector(!!sym(raw_col), base = 5),
        100 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 1000 ~ 
          randomly_round_vector(!!sym(raw_col), base = 10),
        1000 <= abs(!!sym(raw_col)) ~ 
          randomly_round_vector(!!sym(raw_col), base = 100)
      ))
    } else {
      # make seeds
      sort_remove_nas = function(x){
        x = unname(x[!is.na(x)])
        return(paste(sort(x), collapse = " "))
      }
      concated = dplyr::select(df, dplyr::all_of(stable_across_cols)) %>%
        apply(MARGIN = 1, sort_remove_nas)
      
      df$tmp_concatenated = concated
      df$tmp_hashed = sapply(df$tmp_concatenated, digest::digest)  
      df = dplyr::mutate(df, tmp_seed = digest::digest2int(tmp_hashed))
      # round
      df = df %>%
        dplyr::mutate(!!sym(conf_col) := case_when(
          0 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 19 ~ 
            randomly_round_vector(!!sym(raw_col), base = 3, seeds = tmp_seed),
          19 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 20 ~ 
            randomly_round_vector(!!sym(raw_col), base = 2, seeds = tmp_seed),
          20 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 100 ~ 
            randomly_round_vector(!!sym(raw_col), base = 5, seeds = tmp_seed),
          100 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 1000 ~ 
            randomly_round_vector(!!sym(raw_col), base = 10, seeds = tmp_seed),
          1000 <= abs(!!sym(raw_col)) ~ 
            randomly_round_vector(!!sym(raw_col), base = 100, seeds = tmp_seed)
        )) %>%
        select(-tmp_concatenated, -tmp_seed, -tmp_hashed)
    }
    
  } # end for loop
  
  return(df)
}

## suppress small counts ------------------------------------------------------
#'
#' Suppresses values where the count is too small.
#' Values in suppress_cols are replaced with NA if the corresponding value
#' in count_cols is less than the threshold. count_cols is assumed to contain
#' counts of unit records.
#' 
#' If multiple columns are provided then values are suppressed if any count
#' is beneath the threshold.
#'
apply_small_count_suppression <- function(df, suppress_cols, threshold, count_cols = suppress_cols){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.numeric(threshold), "input [threshold] must be of type numeric")
  assert(all(suppress_cols %in% colnames(df)), "[suppress_cols] must be column names of [df]")
  assert(all(count_cols %in% colnames(df)), "[count_cols] must be column names of [df]")
  
  # table of count/threshold values (handled separate threshold per cout column)
  threshold_tbl = data.frame(col = count_cols, threshold = threshold, stringsAsFactors = FALSE)
  
  # suppress
  for(ii in 1:nrow(threshold_tbl)){
    ref = c(threshold_tbl[ii,])
    
    ind_suppress = df[[ref$col]] < ref$threshold
    ind_suppress = sapply(ind_suppress, isTRUE)
    df[ind_suppress, suppress_cols] = NA
  }
  
  return(df)
}

## confidentialise results ----------------------------------------------------
#'
#' Accepts a table in long-thin format output by summarise and label and
#' applies confidentialisation rules:
#' - random rounding 
#' - suppression of small values
#' - ensures consistency between RR and suppression
#' 
confidentialise_results <- function(df,
                                    stable_RR = FALSE,
                                    sum_RR = FALSE,
                                    BASE = 3,
                                    COUNT_THRESHOLD = 6,
                                    SUM_THRESHOLD = 20){
  # checks
  assert(has_long_thin_format(df), "[df] is not in expected format")
  assert(any(c("distinct", "count") %in% colnames(df)), "[df] must have column distinct, count, or both")
  assert(is.logical(stable_RR), "[stable_RR] must be type logical")
  assert(is.logical(sum_RR), "[sum_RR] must be type logical")
  
  # stability
  if(stable_RR){
    col00_val00 = grepl("^(col|val)[0-9][0-9]$", colnames(df))
    summary_var = grepl("^summarised_var$", colnames(df))
    stable_across_cols = colnames(df)[col00_val00 | summary_var]
  } else {
    stable_across_cols = NULL
  }
  
  # random rounding
  if(sum_RR){
    RR_columns = c("distinct", "count", "sum")
  } else {
    RR_columns = c("distinct", "count")
  }
  RR_columns = RR_columns[RR_columns %in% colnames(df)]
  
  df = apply_random_rounding(df = df, RR_columns = RR_columns, BASE = BASE, stable_across_cols = stable_across_cols)
  
  # suppression of counts
  count_cols = c("raw_distinct", "raw_count")
  count_cols = count_cols[count_cols %in% colnames(df)]
  
  num_cols = c("conf_distinct", "conf_count")
  num_cols = num_cols[num_cols %in% colnames(df)]
  
  if(length(num_cols) >= 1){
    df = apply_small_count_suppression(df = df, suppress_cols = num_cols, threshold = COUNT_THRESHOLD, count_cols = count_cols)
  }
  
  # suppression of sums
  if("sum" %in% colnames(df)){
    df = df %>%
      dplyr::mutate(conf_sum = sum) %>%
      dplyr::rename(raw_sum = sum)
  }
  
  if("conf_sum" %in% colnames(df)){
    df = apply_small_count_suppression(df = df, suppress_cols = "conf_sum", threshold = SUM_THRESHOLD, count_cols = count_cols)
    
    # check rounding and suppression consistency
    for(ii in 1:length(num_cols)){
      count_col = dplyr::sym(count_cols[ii])
      num_col = dplyr::sym(num_cols[ii])
      
      # if raw >= threshold and conf < threshold, enforce rounding down so conf >= threshold
      df = dplyr::mutate(df, !!num_col := ifelse(!!num_col < SUM_THRESHOLD & !!count_col >= SUM_THRESHOLD, !!num_col + BASE, !!num_col))
      # if raw < threshold and conf >= threshold, enforce rounding down so conf < threshold
      df = dplyr::mutate(df, !!num_col := ifelse(!!num_col >= SUM_THRESHOLD & !!count_col < SUM_THRESHOLD, !!num_col - BASE, !!num_col))
    }
  }
  
  # conclude
  assert(has_long_thin_format(df), "output not long-thin formatted as expected")
  return(df)
}
