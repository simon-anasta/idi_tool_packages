###############################################################################
#' Description: dbplyr support functions for R development with SQL server
#'
#' Input: Connection details that must be manually set.
#'
#' Output: support functions for ease of using R to manipulate SQL tables.
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: odbc, DBI, dplyr, dbplyr packages, utility_functions.R
#'
#' Notes:
#' - Connection details (line ~35) NOT FOR RELEASE!!
#' - Some functions (and tests) assume SQL Server environment
#'   due to differences in syntax between different implementations of SQL.
#' - Uses code folding by headers (Alt + O to collapse all)
#' - Example use included at end of file.
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-11-18 SA v2 for release
#' 2020-07-12 SA addition of collapse indicator column function
#' 2020-02-13 SA addition of pivot function
#' 2020-01-09 SA v1 split dbplyr support out from utility_functions.R
#' #############################################################################

# # connection details
# DEFAULT_SERVER <- "[]"
# DEFAULT_DATABASE <- "[]"
# DEFAULT_PORT <- NA
# # DO NOT RELEASE THESE VALUES
# # DO NOT RELEASE THESE VALUES
# 
# # error if connection details are missing
# if (is.na(DEFAULT_SERVER) | nchar(DEFAULT_SERVER) < 4 |
#     is.na(DEFAULT_DATABASE) | nchar(DEFAULT_DATABASE) < 4) {
#   stop("Default server and database must be set in dbplyr_helper_functions.R")
# }
# 
# # library
# library(DBI)
# library(dplyr)
# library(dbplyr)

## Add clustered index to a table -----------------------------------------------------------------
#'
#' Note: at most a single clustered index can be added to each table.
#' This operation is potentially expensive, so should be used only where needed.
#' 
#' For researcher created tables non-clustered indexes are recommended.
#' This function only provided for backwards compatibility.
#'
create_clustered_index <- function(db_connection, db, schema, tbl_name, index_columns) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  # table in connection
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  # columns are in table
  assert(
    all(index_columns %in% colnames(create_access_point(db_connection, db, schema, tbl_name))),
    "database table does not have the required columns"
  )
  
  index_columns <- sapply(index_columns, add_delimiters, delimiter = "[]")
  index_columns <- paste0(index_columns, collapse = ", ")
  
  query <- glue::glue("CREATE CLUSTERED INDEX my_index_name ON {db}.{schema}.{tbl_name} ({index_columns})")
  
  # print(query)
  save_to_sql(query, "add_clustered_index")
  result <- DBI::dbExecute(db_connection, as.character(query))
}

## EXAMPLE USAGE ##############################################################
# 2020-03-18
#
# # source
# > source('utility_functions.R')
# > source('dbplyr_helper_functions.R")
#
# # setup
# > our_database = "[our_database]"
# > our_schema = "[our_schema]"
# > db_con = create_database_connection(database = our_database)
#
# # connect
# > my_table = create_access_point(db_connection =  db_con, db = our_database, schema =  our_schema, tbl_name = "example_table")
#
# # use
# > my_table = my_table %>%
#     filter(name == 'James Bond') %>%
#     mutate(code = '007') %>%
#     select(name, code)
#
# # check underlying SQL code
# > my_table %>% show_query()
#
# # fetch data into R
# > my_table = my_table %>% collect()
#
# # close connection
# > close_database_connection(db_con)
#' #############################################################################
