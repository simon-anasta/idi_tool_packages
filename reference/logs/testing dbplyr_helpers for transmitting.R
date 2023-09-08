## initial setup ----------------------------------------------------------------------------------

folder = "~/Network-Shares/IDI-Input/IDI efficiency/testing"
setwd(folder)

# install package from file
# install.packages("./dbplyr.helpers_0.1.0.tar.gz", repos = NULL, type = "source")

# connect
library(DBI)

conn_str = "SUPRESSED"
db_conn = dbConnect(odbc::odbc(), .connection_string = conn_str, timeout = 10)
DBI::dbDisconnect(db_conn)

## variation of test function ---------------------------------------------------------------------
test_with_sql_server2 = function(
  connection_string,
  table_db = "[IDI_Sandpit]",
  view_db = "[IDI_UserCode]",
  our_schema,
  query_path = NA
){
  # validate inputs
  stopifnot(is.character(connection_string))
  # warn_if_missing_delimiters(table_db, our_schema, "[]")
  # warn_if_missing_delimiters(view_db, "[]", "[]")
  
  testing_env = testthat::test_env()
  
  # with(testing_env, {library(dbplyr.helpers)})
  
  assign("connection_string", connection_string, envir = testing_env)
  assign("table_db", table_db, envir = testing_env)
  assign("view_db", view_db, envir = testing_env)
  assign("our_schema", our_schema, envir = testing_env)
  assign("query_path", getwd(), envir = testing_env)
  
  # path
  path = system.file("extdata", "SQL_server_tests", package = "dbplyr.helpers")
  
  # test
  testthat::test_dir(path, env = testing_env)
}

## reference --------------------------------------------------------------------------------------
# 
# testsel <- test_env()
# with(testsel, rsel.opt <- list(nativeEvents = FALSE))
# 
# test_dir("./inst/apps/shinytestapp/tests/", env = testsel)

## execution --------------------------------------------------------------------------------------

test_with_sql_server2(
  connection_string = conn_str,
  table_db = "[IDI_Sandpit]",
  view_db = "[IDI_UserCode]",
  our_schema = "[DL-MAA2023-999]",
  query_path = getwd()
)

## references -------------------------------------------------------------------------------------

# environments for testing
# https://github.com/r-lib/testthat/issues/126

# assigning values between environments
# https://stackoverflow.com/questions/57454085/assign-a-variable-in-r-using-another-variable
# https://stackoverflow.com/questions/9965577/copy-move-one-environment-to-another
# https://stackoverflow.com/questions/34050094/assign-values-to-part-of-an-object-in-another-environment-in-r

## error messages ---------------------------------------------------------------------------------

# Error (test_connect_read_write.R:23:3): connection can see tables in database
# Error in `close_database_connection(db_conn)`: could not find function "close_database_connection"
#
# - probably needs dbplyr.helpers:: on all the library calls


Error (test_connect_read_write.R:40:3): dbplyr writes, reads, and deletes
Error: Can't unquote "IDI_Sandpit"."DL-MAA2023-999"."test2777647"

Backtrace:
  1. dbplyr.helpers::copy_r_to_sql(...)
       at test_connect_read_write.R:40:2
  5. DBI::dbWriteTable(db_connection, this_ID, r_table_name)
  7. odbc::dbWriteTable(...)
  8. odbc (local) .local(conn, name, value, ...)
 10. odbc::dbExistsTable(conn, name)
 13. DBI::dbUnquoteIdentifier(conn, name)


Error (test_manipulations.R:29:3): union row-binds
Error: Can't unquote "IDI_Sandpit"."DL-MAA2023-999"."test6829921"

Error (test_manipulations.R:66:3): pivot replicates tidyr::spread
Error: Can't unquote "IDI_Sandpit"."DL-MAA2023-999"."test6597932"

Error (test_manipulations.R:101:3): collapse indicator columns runs for remote data frames
Error: Can't unquote "IDI_Sandpit"."DL-MAA2023-999"."4217154"

Error (test_manipulations.R:131:3): collapse indicator columns runs for remote data frames with multiple yes values
Error: Can't unquote "IDI_Sandpit"."DL-MAA2023-999"."test4482518"

Error (test_other_writing.R:35:3): sql accepts creation and appending
<odbc::odbc_error/C++Error/error/condition>
Error: <SQL> 'CREATE TABLE [IDI_Sandpit].[DL-MAA2023-999].[test_tbl2394347](
  [number] [int] NOT NULL,
  [date] [date] NOT NULL,
  [character] [varchar](25) NULL
)'
  nanodbc/nanodbc.cpp:1587: 42S01: [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]There is already an object named 'test_tbl2394347' in the database. 
Backtrace:
 1. dbplyr.helpers::create_table(...)
      at test_other_writing.R:35:2
 3. DBI::dbExecute(db_connection, as.character(sql_query))
 5. odbc::dbSendStatement(conn, statement, ...)
 6. odbc:::OdbcResult(connection = conn, statement = statement)
 7. odbc:::new_result(connection@ptr, statement)

