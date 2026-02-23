
####################################################################################

test_that("calcNthNearestCDEC returns expected structure", {
  df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
  result <- calcNthNearestCDEC(df)
  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_s3_class(result[[1]], "data.frame")
  expect_true("cdecGage" %in% names(result[[1]]))
})

test_that("calcNthNearestCDEC produces a warning if n has length > 1", {
  df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
  # Multiple warnings expected: deprecation and n length
  expect_warning(
    expect_warning(result <- calcNthNearestCDEC(df, n=c(1,2)), "deprecated"),
    "n has length > 1"
  )
  expect_type(result, "list")
})

test_that("calcNearestCDEC returns expected structure", {
  df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
  result <- calcNearestCDEC(df)
  expect_s3_class(result, "data.frame")
  expect_true("cdecGage" %in% names(result))
})

####################################################################################
