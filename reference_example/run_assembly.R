################################################################################
# Description: Example run assembly
# 
# - Demonstration only. Not run.
# - Uses code folding by headers (Alt + O to collapse all)
#
################################################################################

## parameters ------------------------------------------------------------- ----

# file paths
POPULATION_FILE = "population_and_period.csv"
MEASURES_FILE = "measures.csv"

# outputs
OUTPUT_DATABASE = "[IDI_Sandpit]"
OUTPUT_SCHEMA = "[DL-MAA20XX-YY]"
LONG_THIN_TABLE_NAME = "[tmp_long_thin_assembled]"
RECTANGULAR_TABLE_NAME = "[tmp_rectangular_assembled]"

# controls
DEVELOPMENT_MODE = TRUE # {TRUE for testing, FALSE for production}
RUN_CHECKS_ONLY = FALSE # {TRUE for testing inputs without assembly}

## library ---------------------------------------------------------------- ----

library(assembly.tool)

## get assembly.tool example ---------------------------------------------- ----

# provide_assembly_tool_example(folder = getwd())

## database connection ---------------------------------------------------- ----

con_str = "DRIVER=ODBC Driver 17 for SQL Server; Trusted_Connection=Yes; SERVER={server},{port}; DATABASE={database};"
db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = con_str)

## assembly --------------------------------------------------------------- ----

dataset_assembly_tool(
  population_control_table = POPULATION_FILE,
  measures_control_table = MEASURES_FILE,
  db_connection = db_conn,
  output_database = OUTPUT_DATABASE,
  output_schema = OUTPUT_SCHEMA,
  output_table_long = LONG_THIN_TABLE_NAME,
  output_table_rectangular = RECTANGULAR_TABLE_NAME,
  control_development_mode = DEVELOPMENT_MODE,
  control_run_checks_only = RUN_CHECKS_ONLY,
  control_silence_progress = FALSE,
  control_append_long_thin = FALSE,
  query_path = NA
)

## disconnect ------------------------------------------------------------- ----

DBI::dbDisconnect(db_conn)
