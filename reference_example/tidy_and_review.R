################################################################################
# Description: Example tidy and review
# 
# - Demonstration only. Not run.
# - Uses code folding by headers (Alt + O to collapse all)
#
################################################################################

## parameters ------------------------------------------------------------- ----

# inputs
DATABASE = "[IDI_Sandpit]"
SCHEMA = "[DL-MAA20XX-YY]"
TABLE = "[tmp_rectangular]"

# outputs
OUT_TABLE = "[tmp_tidy]"
IN_REPORT = "raw_table_report"
OUT_REPORT = "tidy_table_report"

# controls
DEVELOPMENT_MODE = TRUE # {TRUE for testing, FALSE for production}

## library ---------------------------------------------------------------- ----

library(dplyr)
library(dbplyr.helpers)
library(check.dataset)

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

## error checking --------------------------------------------------------- ----
run_time_inform_user("error checks")

# confirm one person per period
assert_all_unique(working_table, c('identity_column', 'label_summary_period'))

# at least 1000 rows
if(!DEVELOPMENT_MODE){
  assert_size(working_table, ">", 1000)
}

## make report on input table --------------------------------------------- ----

if(DEVELOPMENT_MODE){
  run_time_inform_user("make input report")
  explore_report(working_table, id_column = "identity_column", output_file = IN_REPORT)
}

## dataset cleaning ------------------------------------------------------- ----

tidy_table = working_table %>%
  # filter to study population
  filter(
    is_alive == 1,
    resident == 1
  ) %>%
  
  # combine columns that were assembled using HISTOGRAM
  collapse_indicator_columns(prefix = "sex_code=", yes_values = 1, label = "sex_code") %>%
  collapse_indicator_columns(prefix = "region_code=", yes_values = 1, label = "region_code") %>%
  
  # handle missing values
  mutate(
    is_beneficiary = ifelse(coalesce(days_benefit,0) > 185, 1, 0)
  ) %>%
  
  # drop unrequired columns
  select(-label_identity,
         -summary_period_start_date,
         -summary_period_end_date
  ) %>%

  # age at end of 2020
  mutate(age = 2020 - birth_year) %>%
  
  # age categories (separate mutate ensures dbplyr translation of `age` exists)
  mutate(age_cat = case_when(
    00 <= age & age < 20 ~ "00_to_19",
    20 <= age & age < 40 ~ "20_to_39",
    40 <= age & age < 60 ~ "40_to_59",
    60 <= age ~ "60_up"
  )) %>%
  
  # total income
  mutate(
    total_income = round(coalesce(income_taxible,0),2) + 
      round(coalesce(income_nontaxible,0),2) + 
      round(coalesce(income_taxcredits,0),2)
  )

## review dbplyr translation ---------------------------------------------- ----
run_time_inform_user("save translation")

save_to_sql_script(
  query = dplyr::show_query(tidy_table),
  desc = "tidy_table_translation",
  query_path = getwd()
)

## make report on output table -------------------------------------------- ----

if(DEVELOPMENT_MODE){
  run_time_inform_user("make output report")
  explore_report(tidy_table, id_column = "identity_column", output_file = OUT_REPORT)
}

## save tidy table to database -------------------------------------------- ----

run_time_inform_user("save to database")
written_tbl = write_to_database(tidy_table, db_conn, DATABASE, SCHEMA, OUT_TABLE, OVERWRITE = TRUE)

run_time_inform_user("indexing")
create_nonclustered_index(db_conn, DATABASE, SCHEMA, OUT_TABLE, "identity_column")

run_time_inform_user("compressing")
compress_table(db_conn, DATABASE, SCHEMA, OUT_TABLE)

## disconnect ------------------------------------------------------------- ----

DBI::dbDisconnect(db_conn)
run_time_inform_user("complete")
