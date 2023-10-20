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

## random rounding -------------------------------------------------------- ----
#' Apply random rounded
#' 
#' Randomly rounds a numeric vector to the given base.
#' For example, if base is 3 (default) then:
#' * 3 & 6 will be left as is because they are already base 3;
#' * 4 & 7 will be rounded down with prob 2/3 and up with prob 1/3;
#' * 5 & 8 will be rounded down with prob 1/3 and up with prob 2/3.
#' 
#' @param input_vector a numeric array to randomly round.
#' @param base the base to round to. defaults to 3.
#' @param seeds an optional column of seed values, with length equal to
#' `input_vector`. If provided, these seeds are used to determine the random
#' rounding - useful for consistent or reproducible rounding.
#' 
#' @return the input vector with all values randomly rounded so they are
#' divisible by `base`.
#' 
#' @export
randomly_round_vector <- function(input_vector, base = 3, seeds = NULL){
  # check vector is numeric
  stopifnot(is.numeric(input_vector))
  stopifnot(is.numeric(base))
  
  if(is.null(seeds)){
    probs = stats::runif(length(input_vector))
  } else {
    stopifnot(is.numeric(seeds))
    stopifnot(length(input_vector) == length(seeds))
    probs = sapply(seeds, function(s){set.seed(s); stats::runif(1)})
  }
  
  remainder_vector = input_vector %% base
  prob_round_down = (base - remainder_vector) / base
  ind_round_down = probs < prob_round_down
  
  output_vector = input_vector + base - remainder_vector - base * ind_round_down 
}

## apply random rounding -------------------------------------------------- ----
#' Apply random rounding to data frame
#' 
#' Applies random rounding to specified columns.
#' Creates raw_ and conf_ columns so original values are preserved.
#'
#' @param df the data.frame to apply random rounding to.
#' @param RR_columns a column or columns of `df` to be rounded.
#' @param BASE the base to round to. Defaults to 3.
#' @param stable_across_cols an optional array of column names. If provided,
#' these column names are used to generate seeds for the rounding. This
#' enables consistent or reproducible rounding.
#' 
#' @return the data.frame with the columns in `RR_columns` renamed with a
#' `raw_*` prefix, and a matching column with a `conf_*` prefix that contains
#' randomly rounded values.
#' 
#' @export
apply_random_rounding <- function(df, RR_columns, BASE = 3, stable_across_cols = NULL){
  # checks
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.numeric(BASE))
  stopifnot(all(RR_columns %in% colnames(df)))
  # check stable_across_cols if provided
  if(!is.null(stable_across_cols)){
    stopifnot(all(stable_across_cols %in% colnames(df)))
    stopifnot(!any(stable_across_cols %in% RR_columns))
  }
  
  # loop through column types
  for(this_col in RR_columns){
    # value for this iteration
    raw_col = paste0("raw_", this_col)
    conf_col = paste0("conf_", this_col)
    
    # make raw_* column
    colnames(df)[colnames(df) == this_col] = raw_col
    
    # handle seeds
    seeds = NULL
    
    if(!is.null(stable_across_cols)){
      # make seeds
      sort_remove_nas = function(x){
        x = unname(x[!is.na(x)])
        return(paste(sort(x), collapse = " "))
      }
      
      label_columns = dplyr::select(df, dplyr::all_of(stable_across_cols))
      concated = apply(label_columns, MARGIN = 1, sort_remove_nas)
      
      tmp_hashed = sapply(concated, digest::digest)  
      seeds = digest::digest2int(tmp_hashed)
    }
    
    # make conf_* column
    df[[conf_col]] = randomly_round_vector(df[[raw_col]], base = BASE, seeds = seeds)
    
  } # end for loop
  
  return(df)
}

## apply graduated random rounding ---------------------------------------- ----
#' Apply graduated random rounding to data frame
#' 
#' Applies graduated random rounding (GRR) to specified columns.
#' Creates raw_ and conf_ columns so original values are preserved.
#' 
#' Thresholds for graduation are set by the Stats NZ microdata output guide.
#' Only apply one of GRR or RR3. Applyng both will cause errors.
#' 
#' @param df the data.frame to apply graduated random rounding to.
#' @param GRR_columns a column or columns of `df` to be rounded.
#' @param stable_across_cols an optional array of column names. If provided,
#' these column names are used to generate seeds for the rounding. This
#' enables consistent or reproducible rounding.
#' 
#' @return the data.frame with the columns in `GRR_columns` renamed with a
#' `raw_*` prefix, and a matching column with a `conf_*` prefix that contains
#' the rounded values.
#' 
#' @export
apply_graduated_random_rounding <- function(df, GRR_columns, stable_across_cols = NULL){
  # checks
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(all(GRR_columns %in% colnames(df)))
  # check stable_across_cols if provided
  if(!is.null(stable_across_cols)){
    stopifnot(all(stable_across_cols %in% colnames(df)))
    stopifnot(!any(stable_across_cols %in% GRR_columns))
  }
  
  # loop through column types
  for(this_col in GRR_columns){
    # value for this iteration
    raw_col = paste0("raw_", this_col)
    conf_col = paste0("conf_", this_col)
    
    # make raw_* column
    colnames(df)[colnames(df) == this_col] = raw_col
    
    # handle seeds
    seeds = NULL
    
    if(!is.null(stable_across_cols)){
      # make seeds
      sort_remove_nas = function(x){
        x = unname(x[!is.na(x)])
        return(paste(sort(x), collapse = " "))
      }
      
      label_columns = dplyr::select(df, dplyr::all_of(stable_across_cols))
      concated = apply(label_columns, MARGIN = 1, sort_remove_nas)
      
      tmp_hashed = sapply(concated, digest::digest)  
      seeds = digest::digest2int(tmp_hashed)
    }
    
    # make conf_* column
    df[[conf_col]] = dplyr::case_when(
      0 <= abs(df[[raw_col]]) & abs(df[[raw_col]]) < 19 ~ 
        randomly_round_vector(df[[raw_col]], base = 3, seeds = seeds),
      19 <= abs(df[[raw_col]]) & abs(df[[raw_col]]) < 20 ~ 
        randomly_round_vector(df[[raw_col]], base = 2, seeds = seeds),
      20 <= abs(df[[raw_col]]) & abs(df[[raw_col]]) < 100 ~ 
        randomly_round_vector(df[[raw_col]], base = 5, seeds = seeds),
      100 <= abs(df[[raw_col]]) & abs(df[[raw_col]]) < 1000 ~ 
        randomly_round_vector(df[[raw_col]], base = 10, seeds = seeds),
      1000 <= abs(df[[raw_col]]) ~ 
        randomly_round_vector(df[[raw_col]], base = 100, seeds = seeds)
    )
    
  } # end for loop
  
  return(df)
}

## suppress small counts -------------------------------------------------- ----
#' Apply suppression to data frame
#' 
#' Applies suppression to a data frame. Where `count_cols` are less than
#' `threshold`, replace `suppress_cols` with `NA`.
#' 
#' @param df the data.frame to apply suppression to.
#' @param suppress_cols the name of the columns to apply suppression to.
#' @param threshold the minimum acceptable count. Values less than (but not
#' equal to) the threshold are suppressed.
#' @param count_cols the names of the columns to compare with the threshold.
#' Optional, defaults to `suppress_cols` if not provided. The intention is that
#' `count_cols` contains the counts of unit records.
#' 
#' @return the data.frame with the `suppress_cols` suppressed where the count
#' is below the threshold.
#' 
#' @details
#' Multiple columns and thresholds can be provided. If multiple `suppress_cols`
#' are given then all receive the same suppression. If multiple `count_cols`
#' are given then suppression is applied if any count is beneath the threshold.
#' Multiple `thresholds` can only be provided if multiple `count_cols` are
#' provided, each count col will use the corresponding threshold.
#' 
#' @export
apply_small_count_suppression <- function(df, suppress_cols, threshold, count_cols = suppress_cols){
  # checks
  stopifnot(is.data.frame(df) | dplyr::is.tbl(df))
  stopifnot(is.numeric(threshold))
  stopifnot(all(suppress_cols %in% colnames(df)))
  stopifnot(all(count_cols %in% colnames(df)))
  stopifnot(length(threshold) == 1 | length(threshold) == length(count_cols))
  
  # table of count/threshold values (handled separate threshold per count column)
  threshold_tbl = data.frame(col = count_cols, threshold = threshold, stringsAsFactors = FALSE)
  
  # suppress
  for(ii in 1:nrow(threshold_tbl)){
    this_col = threshold_tbl$col[ii]
    this_threshold = threshold_tbl$threshold[ii]
    
    ind_suppress = df[[this_col]] < this_threshold
    ind_suppress = sapply(ind_suppress, isTRUE)
    df[ind_suppress, suppress_cols] = NA
  }
  
  return(df)
}

## confidentialise results ------------------------------------------------ ----
#' Apply common confidentiality to long-thin output
#' 
#' Applies the most common confidentiality rules to long-thin output. Designed
#' to confidentialise in a single command the output from `summarise_and_label`
#' and `summarise_and_label_over_lists`.
#' 
#' * Applies random rounding to columns `count` and `distinct` if they exist.
#' * Applies suppression to columns `count` and `distinct` if they exist.
#' * Applies suppression to column `sum` if it exists
#' * Ensures consistency between random rounding and suppression (e.g. if a
#' count is above/below a threshold then it will remain above/below the
#' threshold after rounding).
#' 
#' @param df the data.frame to apply random rounding to. Must be in long-thin
#' format.
#' @param stable_RR T/F, whether to apply stable random rounding using the
#' non-numeric columns. Defaults to FALSE.
#' @param sum_RR T/F, whether to randomly round the `sum` column (if present
#' in `df`). Defaults to FALSE. Should be used when the sum is a type of count.
#' For example, "sum the total number of events for people" is equivalent to
#' counting all events.
#' @param BASE the base to randomly round to. Defaults to 3.
#' @param COUNT_THRESHOLD the minimum acceptable count. Where `count` or
#' `distinct` are below this threshold then they will be suppressed. Defaults
#' to 6.
#' @param SUM_THRESHOLD the minimum accepted count to output a sum. If either
#' `count` or `distinct` are below this threshold, then the sum column will
#' be suppressed. Defaults to 20.
#' 
#' @return the data.frame (still in long-thin format) with the numeric columns
#' renamed with a `raw_*` prefix, and a matching column with a `conf_*` prefix
#' that contains the confidentialised values.
#' 
#' @export
confidentialise_results <- function(
    df,
    stable_RR = FALSE,
    sum_RR = FALSE,
    BASE = 3,
    COUNT_THRESHOLD = 6,
    SUM_THRESHOLD = 20
){
  # checks
  stopifnot(has_long_thin_format(df))
  stopifnot(any(c("distinct", "count") %in% colnames(df)))
  stopifnot(is.logical(stable_RR))
  stopifnot(is.logical(sum_RR))
  
  # stability
  stable_across_cols = NULL
  if(stable_RR){
    col00_val00 = grepl("^(col|val)[0-9][0-9]$", colnames(df))
    summary_var = grepl("^summarised_var$", colnames(df))
    stable_across_cols = colnames(df)[col00_val00 | summary_var]
  }
  
  # random rounding
  RR_columns = c("distinct", "count")
  if(sum_RR){
    RR_columns = c(RR_columns, "sum")
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
    df = dplyr::mutate(df, conf_sum = sum)
    df = dplyr::rename(df, raw_sum = sum)
  }
  
  if("conf_sum" %in% colnames(df)){
    df = apply_small_count_suppression(df = df, suppress_cols = "conf_sum", threshold = SUM_THRESHOLD, count_cols = count_cols)
    
    # check rounding and suppression consistency
    for(ii in 1:length(num_cols)){
      count_col = count_cols[ii]
      num_col = num_cols[ii]
      
      # if raw >= threshold and conf < threshold, enforce rounding down so conf >= threshold
      df[[num_col]] = ifelse(df[[num_col]] < SUM_THRESHOLD & df[[count_col]] >= SUM_THRESHOLD, df[[num_col]] + BASE, df[[num_col]])
      # if raw < threshold and conf >= threshold, enforce rounding down so conf < threshold
      df[[num_col]] = ifelse(df[[num_col]] >= SUM_THRESHOLD & df[[count_col]] < SUM_THRESHOLD, df[[num_col]] - BASE, df[[num_col]])
    }
  }
  
  # conclude
  stopifnot(has_long_thin_format(df))
  return(df)
}
