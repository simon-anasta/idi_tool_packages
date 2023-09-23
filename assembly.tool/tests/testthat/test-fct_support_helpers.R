################################################################################
# Description: Automated tests for assembly tool support functions
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
# - Designed for MS SQL Server will require editing for other SQL flavours
#
# Issues:
#
################################################################################

## read_table_file(file_name_and_path) ------------------------------------ ----

test_that("all (delimiated) file formats read", {
  expected_df = data.frame(
    sql = c("[foo]", "[bar]"),
    txt = c('"qwe"', '"asd"'),
    stringsAsFactors = FALSE
  )
  
  path = system.file("extdata", "testing", package = "assembly.tool")
  
  csv_read = read_table_file(file.path(path, "read_table_test.csv"))
  xls_read = read_table_file(file.path(path, "read_table_test.xls"))
  xlsx_read = read_table_file(file.path(path, "read_table_test.xlsx"))
  
  expect_true(all.equal(csv_read, expected_df))
  expect_true(all.equal(xls_read, expected_df))
  expect_true(all.equal(xlsx_read, expected_df))
})

## has_internal_delimiters(string, table_label) --------------------------- ----

test_that("warn only for internal delimiters", {
  
  expect_true(has_internal_delimiters("[str].[ing]"))
  expect_true(has_internal_delimiters("[str]ing]"))
  
  for (string in c("[string]", "\"string\"", "string")) {
    expect_false(has_internal_delimiters(string))
  }
})

## prep_for_sql(string, alias) -------------------------------------------- ----

test_that("double quotes removed", {
  expect_equal(prep_for_sql('"foobar"', "k"), "'foobar'")
  expect_equal(prep_for_sql("\"foobar\"", "k"), "'foobar'")
})

test_that("alias appended", {
  expect_equal(prep_for_sql("[foobar]", "k"), "k.[foobar]")
})

## handle_summary_case(summary_type, proportional, m_label, m_value, m_start_date, m_end_date, p_start_date, p_end_date) ----

test_that("summary case gives required outputs", {
  list_out = handle_summary_case(
    "SUM", FALSE, "m_label", "m_value",
    "2001-01-01", "2001-03-01", "2001-02-01", "2001-06-01"
  )
  req_names = c("label", "value", "group")
  
  expect_true(all(req_names %in% names(list_out)))
  expect_true(all(names(list_out) %in% req_names))
})

test_that("proportional calculates correctly", {
  LVG = handle_summary_case("SUM", TRUE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  received_value = LVG$value
  expected_value = paste0(
    "1.0 * (1+DATEDIFF(DAY, IIF(m_start < p_start, p_start, m_start), IIF(m_end < p_end, m_end, p_end)))",
    " / (1+DATEDIFF(DAY, m_start, m_end))"
  )
  
  received_value = tolower(gsub("[[:space:]]", "", received_value))
  expected_value = tolower(gsub("[[:space:]]", "", expected_value))
  
  expect_match(received_value, expected_value, fixed = TRUE)
  
  LVG = handle_summary_case("DURATION", TRUE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_equal(LVG$value, "sum(1+datediff(day,iif(m_start<p_start,p_start,m_start),iif(m_end<p_end,m_end,p_end)))")
  expect_match(LVG$group, "label", fixed = TRUE)
})

test_that("each summary type handles correctly", {
  LVG = handle_summary_case("SUM", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "sum(1.0*value)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("MIN", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "min(1.0*value)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("MAX", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "max(1.0*value)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("COUNT", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "count(value)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("EXISTS", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "iif(count(value)>=1,1,0)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("DURATION", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "sum(1+datediff(day,m_start,m_end))", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("HISTOGRAM", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  expect_equal(LVG$group, c("label", "value"))
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "concat(label,'=',value)", fixed = TRUE)
  expect_match(LVG$value, "count(value)", fixed = TRUE)
  
  LVG = handle_summary_case("DISTINCT", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(LVG)
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "count(distinct value)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  LVG = handle_summary_case("MEAN", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end")
  LVG[] = tolower(gsub("[[:space:]]", "", LVG))
  expect_match(LVG$label, "label", fixed = TRUE)
  expect_match(LVG$value, "avg(1.0*value)", fixed = TRUE)
  expect_match(LVG$group, "label", fixed = TRUE)
  
  expect_error(handle_summary_case("unexcepted input", FALSE, "label", "value", "m_start", "m_end", "p_start", "p_end"))
})
