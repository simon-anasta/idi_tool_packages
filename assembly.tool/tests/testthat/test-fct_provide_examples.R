################################################################################
# Description: Automated tests for assembly tool examples
# Author: Simon Anastasiadis
#
# Notes:
# - Uses code folding by headers (Alt + O to collapse all)
#
# Issues:
#
################################################################################

## provide_assembly_tool_example(folder = NA) ----------------------------- ----

test_that("example copied", {
  
  path = system.file("extdata", "testing_demonstration_example", package = "assembly.tool")
  old_folder = "demonstration_example"
  old_path = file.path(path, old_folder)
  
  if(dir.exists(old_path)){
    unlink(old_path, recursive = TRUE)
  }
  
  provide_assembly_tool_example(folder = path)
  
  expect_true(dir.exists(file.path(path, "demonstration_example")))
  expect_true(file.exists(file.path(path, "demonstration_example", "control_measures.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "control_population_and_period.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "data_accidents.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "data_benefit_payment.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "data_project_population.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "output_long_thin.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "output_rectangular.csv")))
  expect_true(file.exists(file.path(path, "demonstration_example", "run_assembly.R")))
  
  unlink(old_path, recursive = TRUE)
})




