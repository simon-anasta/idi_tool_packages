################################################################################
# Description: Example exploration
# 
# - Demonstration only. Not run.
# - Uses code folding by headers (Alt + O to collapse all)
#
################################################################################

## library ---------------------------------------------------------------- ----

library(dplyr)
library(dbplyr.helpers)
library(check.dataset)

## parameters ------------------------------------------------------------- ----

DATABASE = "[DB]"
SCHEMA = "[SCHEMA]"

## database connection ---------------------------------------------------- ----

# display_connection_guidance()

db_conn = DBI::dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 18 for SQL Server",
  Trusted_Connection = "Yes",
  TrustServerCertificate = "Yes",
  server = "server",
  database = "database"
)

## copy data to SQL ------------------------------------------------------- ----

data("band_instruments")
data("band_members")

copy_r_to_sql(db_conn, DATABASE, SCHEMA, "[members]", band_members, OVERWRITE = TRUE)
copy_r_to_sql(db_conn, DATABASE, SCHEMA, "[instruments]", band_instruments, OVERWRITE = TRUE)

## confirm existence ------------------------------------------------------ ----

table_or_view_exists_in_db(db_conn, DATABASE, SCHEMA, "[members]")
table_or_view_exists_in_db(db_conn, DATABASE, SCHEMA, "[instruments]")

## connect to remote tables ----------------------------------------------- ----

remote_members = create_access_point(db_conn, DATABASE, SCHEMA, "[members]")
remote_instruments = create_access_point(db_conn, DATABASE, SCHEMA, "[instruments]")

## investigate ------------------------------------------------------------ ----

class(band_members)
class(remote_members)

head(remote_members)

# size
num_row(band_instruments)
num_unique_entries(band_instruments, c("name", "plays"))

## join tables ------------------------------------------------------------ ----

# required column exists
table_contains_required_columns(remote_members, "name")
table_contains_required_columns(remote_instruments, "name")

# a one-to-one join requires column contents are unique and every value can join
check_all_unique(remote_members, "name")
check_all_unique(remote_instruments, "name")
check_join_covered(remote_members, remote_instruments, "name")
check_join_covered(remote_instruments, remote_members, "name")
# at least one value is false: join is not one-to-one

remote_joined_band = full_join(
  remote_members,
  remote_instruments,
  by = "name"
)

## review SQL translation ------------------------------------------------- ----

show_query(remote_joined_band)

## pull data back into R -------------------------------------------------- ----

local_joined_band = collect(remote_joined_band)

## delete database tables ------------------------------------------------- ----

delete_table(db_conn, DATABASE, SCHEMA, "[members]")
delete_table(db_conn, DATABASE, SCHEMA, "[instruments]")

# confirm no longer exist
table_or_view_exists_in_db(db_conn, DATABASE, SCHEMA, "[members]")
table_or_view_exists_in_db(db_conn, DATABASE, SCHEMA, "[instruments]")

## disconnect ------------------------------------------------------------- ----

DBI::dbDisconnect(db_conn)
