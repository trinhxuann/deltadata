
####################################################################################

test_that("calcNthNearestCDEC with n=1 is identical to calcNearestCDEC", {
  df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
  result_1 <- calcNearestCDEC(df)
  result_2 <- calcNthNearestCDEC(df)
  testthat::expect_identical(object=result_1, expected=result_2)
})

test_that("calcNthNearestCDEC produces a warning if n has length > 1", {
	df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
	result_1 <- calcNearestCDEC(df)
  testthat::expect_warning({ result_3 <- calcNthNearestCDEC(df, n=c(1,2)) })
  testthat::expect_identical(object=result_1, expected=result_3)
})

####################################################################################
