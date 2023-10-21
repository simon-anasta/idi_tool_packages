################################################################################
# Description: Example summarise and output
# 
# - Demonstration only. Not run.
# - Uses code folding by headers (Alt + O to collapse all)
#
################################################################################

## parameters ------------------------------------------------------------- ----

# inputs
DATABASE = "[IDI_Sandpit]"
SCHEMA = "[DL-MAA20XX-YY]"
TABLE = "[tmp_tidy]"

# outputs
OUTPUT_FOLDER = "../output/"
FILE_SUMMARY = "summarised_results.csv"
FILE_CONFIDENTIALISED = "confidentialised_results.csv"

# controls
DEVELOPMENT_MODE = TRUE # {TRUE for testing, FALSE for production}

## library ---------------------------------------------------------------- ----

library(dplyr)
library(dbplyr.helpers)
library(conf.output)

## database connection ---------------------------------------------------- ----

con_str = "DRIVER=ODBC Driver 17 for SQL Server; Trusted_Connection=Yes; SERVER={server},{port}; DATABASE={database};"
db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = con_str)

## access dataset --------------------------------------------------------- ----
run_time_inform_user("database connect")

working_table = create_access_point(db_conn, DATABASE, SCHEMA, TABLE)

# filter to every 100th row for performance during development
if(DEVELOPMENT_MODE){
  working_table = working_table %>% filter(identity_column %% 100 == 0)
}

## list variables for summarise ------------------------------------------- ----
run_time_inform_user("prep for summarise")

demographics = c("age", "sex_gender", "urban_rural")
ethnicity = c("eth_asian", "eth_euro", "eth_maori", "eth_MELAA", "eth_other", "eth_pacific")
region = "region_code"

demographic_pairs = cross_product_column_names(
  demographics,
  demographics,
  always = region,
  drop.dupes = TRUE
)

demographic_ethnicity = cross_product_column_names(
  demographics,
  ethnicity,
  always = region,
  drop.dupes = TRUE
)

summary_groups = c(demographic_pairs, demographic_ethnicity)

columns_to_count = list("identity_column", "overseas_indicator")
columns_to_sum = list("employment_income", "total_income")

## make summary counts ---------------------------------------------------- ----
run_time_inform_user("summary: count")

count_summary = summarise_and_label_over_lists(
  df = working_table, 
  group_by_list = summary_groups,
  summarise_list = columns_to_count,
  make_distinct = FALSE,
  make_count = TRUE,
  make_sum = FALSE,
  clean = "zero.as.na", # {"none", "na.as.zero", "zero.as.na"}
  remove.na.from.groups = TRUE
)

## make summary sums ------------------------------------------------------ ----
run_time_inform_user("summary: sum")

sum_summary = summarise_and_label_over_lists(
  df = working_table, 
  group_by_list = summary_groups,
  summarise_list = columns_to_sum,
  make_distinct = FALSE,
  make_count = TRUE,
  make_sum = TRUE,
  clean = "zero.as.na", # {"none", "na.as.zero", "zero.as.na"}
  remove.na.from.groups = TRUE
)

## confidentialise summaries ---------------------------------------------- ----
run_time_inform_user("confidentialise summaries")

count_summary_conf = confidentialise_results(count_summary)
sum_summary_conf = confidentialise_results(sum_summary)

## write for output ------------------------------------------------------- ----
run_time_inform_user("writing csv output")

combined_summary = dplyr::bind_rows(count_summary, sum_summary)
combined_conf = dplyr::bind_rows(count_summary_conf, sum_summary_conf)

write.csv(combined_summary, file.path(OUTPUT_FOLDER, FILE_SUMMARY))
write.csv(combined_conf, file.path(OUTPUT_FOLDER, FILE_CONFIDENTIALISED))

## disconnect ------------------------------------------------------------- ----

DBI::dbDisconnect(db_conn)
run_time_inform_user("complete")
