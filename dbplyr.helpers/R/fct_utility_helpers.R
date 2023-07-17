################################################################################
# Description: Utility functions to support R development
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## Inform user with time stamped measure ---------------------------------- ----
#' Print to console time of function call followed by msg.
#' 
#' @param msg a message to display to the user with time stamps.
#' @param context optional, if provided formats the message. Three options:
#' {"heading", "details", "all"}
#' @param print_off if true nothing is printed. Allows for printing of an entire
#' super-function to be turned off.
#' 
#' @export
#' 
run_time_inform_user = function(msg, context = NA, print_off = FALSE) {
  stopifnot(is.na(context) | context %in% c("all", "details", "heading"))
  stopifnot(is.logical(print_off))
  stopifnot(is.character(msg))
  
  if(print_off){
    return(NULL)
  }
  
  # prefix by level
  msg = dplyr::case_when(
    context == "heading" ~ toupper(msg),
    context == "details" ~ paste0(" -- ", msg),
    context == "all" ~ paste0(" ---- ", msg),
    TRUE ~ msg
  )
  
  now = as.character(Sys.time())
  # display
  cat(now, "|", msg, "\n")
}

## Not In ----------------------------------------------------------------- ----
#' Negative of %in%
#'
#' @param x an array of elements
#' @param y an array of elements
#' 
#' @return T/F for each element in `x` whether the element does NOT appear in
#' `y`. Conversion may occur implicitly between ata types.
#' 
"%not_in%" = function(x, y) {
  !("%in%"(x, y))
}

## No special characters -------------------------------------------------- ----
#' No special characters
#' 
#' Check input string for absence of special characters
#' Helps prevent accidental SQL injection
#' 
#' @param in_string a string to be checked for special characters.
#' 
#' No return. Silent pass if input is string and contains no special characters.
#' Otherwise errors if (1) input is not a string, (2) input has length greater
#' than 1, or (3) input string contains special characters.
#' 
#' Special characters are: `;:'(){}?"*` and space.
#' 
no_special_characters = function(in_string) {
  stopifnot(is.character(in_string))
  stopifnot(length(in_string) <= 1)
  
  in_string = strsplit(in_string, "")[[1]]
  
  SPECIAL_CHARACTERS = c(";", ":", "'", "(", ")", "{", "}", "?", "\"", "*", " ")
  stopifnot(all(SPECIAL_CHARACTERS %not_in% in_string))
  
  return(NULL)
}

## Check if delimited ----------------------------------------------------- ----
#' Check string for delimiter
#' 
#' The entries in the input control tables should be delimited as either
#' [] for sql columns or "" for strings
#' This lets us run a check for the right delimiter, e.g.
#' is_delimited(string, "[]")
#' is_delimited(string, "\"")
#' 
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return T/F if the string is delimited
#' 
is_delimited = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) >= 1)
  stopifnot(nchar(delimiter) <= 2)
  
  n_str = nchar(string)
  n_delim = nchar(delimiter)
  
  string_longer_than_delimiters = n_str > n_delim
  first_char_delimited = substr(string, 1, 1) == substr(delimiter, 1, 1)
  last_char_delimited = substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)
  
  return(string_longer_than_delimiters & first_char_delimited & last_char_delimited)
}

## Remove delimiters ------------------------------------------------------ ----
#' Remove delimiters from text string
#' 
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return the string with the specified delimiters removed (if necessary).
#' 
remove_delimiters = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) >= 1)
  stopifnot(nchar(delimiter) <= 2)
  
  n_str = nchar(string)
  n_delim = nchar(delimiter)
  
  first_char_match = substr(string, 1, 1) == substr(delimiter, 1, 1)
  first_char = ifelse(first_char_match, 2, 1)
  last_char_match = substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)
  last_char = ifelse(last_char_match, n_str - 1, n_str)
  
  return(substr(string, first_char, last_char))
}

## Add delimiters --------------------------------------------------------- ----
#' Add delimiters to text string
#' 
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return the string with the specified delimiters added (if necessary).
#' 
add_delimiters = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) >= 1)
  stopifnot(nchar(delimiter) <= 2)
  
  n_str = nchar(string)
  n_delim = nchar(delimiter)
  
  first_char_match = substr(string, 1, 1) == substr(delimiter, 1, 1)
  first_char = ifelse(first_char_match, "", substr(delimiter, 1, 1))
  last_char_match = substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)
  last_char = ifelse(last_char_match, "", substr(delimiter, n_delim, n_delim))
  
  return(paste0(first_char, string, last_char))
}

## Contains additional delimiters ----------------------------------------- ----
#' Checks delimited string contains internal delimiters
#' 
#' For most applications we want to error if an internal delimiter is found.
#' This helps protect against SL injection.
#'
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return the string with the specified delimiters added (if necessary).
#' 
has_internal_delimiters = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) >= 1)
  stopifnot(nchar(delimiter) <= 2)
  
  # trim if string is delimited
  if(is_delimited(string, delimiter)){
    string = substr(string, 2, nchar(string) - 1)
  }
  
  # add single quote if double quote included
  if (grepl("\"", delimiter)) {
    delimiter = paste0(delimiter, "'")
  }
  
  string = strsplit(string, "") %>% unlist(use.names = FALSE)
  delimiter = strsplit(delimiter, "") %>% unlist(use.names = FALSE)
  
  return(any(string %in% delimiter))
}
