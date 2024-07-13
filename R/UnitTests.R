# Unit Tests

library(testthat)
source("C:/Users/siger/Desktop/Universität/FortgeschritteneStatistischeSft/Exercise6/R/Functions.R")

# Example data for testing
example_data <- data.frame(
  name = c("John", "Jane", "Alice"),
  age = c(30, 25, 35),
  city = c("New York", "Los Angeles", "Chicago")
)

# Test cases
test_that("CSV file operations work correctly", {
  temp_csv <- tempfile(fileext = ".csv")
  write_data_file(example_data, temp_csv)
  read_data <- read_data_file(temp_csv)
  expect_equal(as.data.frame(read_data), as.data.frame(example_data))
})

test_that("TSV file operations work correctly", {
  temp_tsv <- tempfile(fileext = ".tsv")
  write_data_file(example_data, temp_tsv)
  read_data <- read_data_file(temp_tsv)
  expect_equal(as.data.frame(read_data), as.data.frame(example_data))
})

test_that("XLSX file operations work correctly", {
  temp_xlsx <- tempfile(fileext = ".xlsx")
  write_data_file(example_data, temp_xlsx)
  read_data <- read_data_file(temp_xlsx)
  expect_equal(as.data.frame(read_data), as.data.frame(example_data))
})

test_that("JSON file operations work correctly", {
  temp_json <- tempfile(fileext = ".json")
  write_data_file(example_data, temp_json)
  read_data <- read_data_file(temp_json)
  expect_equal(as.data.frame(read_data), as.data.frame(example_data))
})

test_that("RDS file operations work correctly", {
  temp_rds <- tempfile(fileext = ".rds")
  write_data_file(example_data, temp_rds)
  read_data <- read_data_file(temp_rds)
  expect_equal(as.data.frame(read_data), as.data.frame(example_data))
})
