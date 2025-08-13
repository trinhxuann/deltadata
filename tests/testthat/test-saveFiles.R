test_that("saveRelationalTables saves tables and returns correct metadata", {
  # withr should be available in a testthat context
  # If not, I can use the manual tempdir creation and cleanup.
  # Let's assume withr is available.
  temp_dir <- withr::local_tempdir()

  # Sample data
  table1 <- data.frame(a = 1, b = 2)
  table2 <- data.frame(x = "hello", y = "world")
  tables <- list(TableA = table1, TableB = table2)

  # Test 1: Basic functionality with default file names
  result1 <- saveRelationalTables(tables, folderPath = temp_dir)

  # Check metadata return
  expect_true(all(result1$writeStatus == "Success"))
  expect_equal(result1$table, c("TableA", "TableB"))

  # Check if files exist and content
  file_a_path <- file.path(temp_dir, "TableA.csv")
  expect_true(file.exists(file_a_path))
  read_table1 <- read.csv(file_a_path)
  expect_equal(read_table1, table1)

  file_b_path <- file.path(temp_dir, "TableB.csv")
  expect_true(file.exists(file_b_path))
  read_table2 <- read.csv(file_b_path, stringsAsFactors = FALSE)
  expect_equal(read_table2, table2)

  # Clean up files for next test within the same temp dir
  file.remove(file_a_path, file_b_path)

  # Test 2: With custom file names
  custom_names <- c("data_a", "data_b")
  result2 <- saveRelationalTables(tables, folderPath = temp_dir, fileNames = custom_names)

  # Check metadata
  expect_true(all(result2$writeStatus == "Success"))
  expect_equal(result2$table, custom_names)

  # Check file existence and content
  custom_file_path <- file.path(temp_dir, "data_a.csv")
  expect_true(file.exists(custom_file_path))
  read_data_a <- read.csv(custom_file_path)
  expect_equal(read_data_a, table1)
})
