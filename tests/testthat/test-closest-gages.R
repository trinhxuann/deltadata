
####################################################################################

test_that("calcNthNearestCDEC with n=1 is identical to calcNearestCDEC", {
  df <- data.frame(station = "306", lat = 38.00064, lon = -122.4136)
  result_1 <- calcNearestCDEC(df)
  result_2 <- calcNthNearestCDEC(df)

  testthat::expect_identical(object=result_1, expected=result_2)
})

####################################################################################
