################################################################################
# Description: Confidentialisation functionality
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed to work with both local/in-memory R data.frames and remote/SQL/
#   dbplyr-translated data sources.
#
# Issues:
#
################################################################################

## Filter to limited number of rows --------------------------------------- ----
#' Filter to a limited number of rows