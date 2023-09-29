################################################################################
# Description: Demonstrate the Dataset Assembly Tool
#
# Uses SQLite database to conduct a basic assembly.
# The source tables, expected output table, and copies of the database tables
# are provided as csv files.
################################################################################

## user setup ------------------------------------------------------------------

# working directory
working_dir = "~/Path/To/Folder/Containing/Control/Files"

# control files
POPULATION_FILE = "control_population_and_period.csv"
MEASURES_FILE = "control_measures.csv"

# outputs
OUTPUT_DATABASE = "[IDI_Sandpit]"
OUTPUT_SCHEMA = "[DL-MAA20YY-XX]"
OUTPUT_LONG_THIN_TABLE = "[demo_assembled_data]"
OUTPUT_RECTANGULAR_TABLE = "[demo_rectangular]"

# controls
RUN_CHECKS_ONLY = FALSE # {TRUE for testing inputs without assembly}
DEVELOPMENT_MODE = TRUE # {TRUE for testing, FALSE for production}

## setup -----------------------------------------------------------------------

setwd(working_dir)
library(assembly.tool)

## database connection - SQLite ------------------------------------------------
# ignores schema during processing



## database connection - SQL Server --------------------------------------------
# demonstration only for ease of reference



## run assembly ----------------------------------------------------------------

dataset_assembly_tool(
    population_control_table = POPULATION_FILE,
    measures_control_table = MEASURES_FILE,
    db_connection,
    output_database = OUTPUT_DATABASE,
    output_schema = OUTPUT_SCHEMA,
    output_table_long = OUTPUT_LONG_THIN_TABLE,
    output_table_rectangular = OUTPUT_RECTANGULAR_TABLE,
    control_development_mode = DEVELOPMENT_MODE,
    control_run_checks_only = RUN_CHECKS_ONLY
)
