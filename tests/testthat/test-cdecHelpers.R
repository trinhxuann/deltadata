test_that("parseDate correctly parses various date formats", {
  expect_equal(parseDate("2023-01-15"), as.Date("2023-01-15"))
  expect_equal(parseDate("01/15/2023"), as.Date("2023-01-15"))
  expect_equal(parseDate("2023/01/15"), as.Date("2023-01-15"))
  expect_equal(parseDate("01-15-2023"), as.Date("2023-01-15"))
  expect_error(parseDate("15-01-2023"))
  expect_error(parseDate("2023-15-01"))
  expect_error(parseDate("not a date"))
})

test_that("estimateCdecRows calculates row estimates correctly", {
  expect_equal(estimateCdecRows(1, "daily", "2023-01-01", "2023-01-01"), 1)
  expect_equal(estimateCdecRows(1, "daily", "2023-01-01", "2023-01-02"), 2)
  expect_equal(estimateCdecRows(2, "daily", "2023-01-01", "2023-01-02"), 4)
  expect_equal(estimateCdecRows(1, "hourly", "2023-01-01", "2023-01-01"), 24)
  expect_equal(estimateCdecRows(1, "event", "2023-01-01", "2023-01-01"), 96)
  expect_error(estimateCdecRows(1, "invalid_duration", "2023-01-01", "2023-01-01"))
})

test_that("findDataStart correctly identifies the start of data in a table", {
  # Test case 1: Clean data frame, header is row 1, data starts at row 2
  df1 <- data.frame(
    V1 = c("Header1", "Data1", "Data2"),
    V2 = c("Header2", "Data3", "Data4")
  )
  expect_equal(findDataStart(df1, headerRow = 1), 2)

  # Test case 2: Data frame with empty rows and varying content density
  df2 <- data.frame(
    A = c("", "Title", "Subtitle", "Header1", "Value1", "Value2", ""),
    B = c("", "", "", "Header2", "Value3", "Value4", "")
  )
  expect_equal(findDataStart(df2, headerRow = 4), 5)

  # Test case 3: More complex case with NA and whitespace
  df3 <- data.frame(
    Col1 = c("Report Title", "", "Date: 2023-01-01", "ID", "1", "2", "3"),
    Col2 = c("", "", "", "Name", "A", "B", "C"),
    stringsAsFactors = FALSE
  )
  expect_equal(findDataStart(df3, headerRow = 4), 5)
})
