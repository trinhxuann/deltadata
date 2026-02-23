test_that("physicalOutliers identifies outliers correctly", {
  x <- c(1, 2, 3, 4, 5, 100)

  # Using mean
  result_mean <- physicalOutliers(x, measure = "mean", sdThreshold = 2)
  expect_true(result_mean$outlier[6])
  expect_false(any(result_mean$outlier[1:5]))

  # Using median
  result_median <- physicalOutliers(x, measure = "median", sdThreshold = 2)
  expect_true(result_median$outlier[6])
  expect_false(any(result_median$outlier[1:5]))

  # Using a different reference vector `y`
  y <- c(1, 2, 3, 4, 5)
  result_y <- physicalOutliers(x, y = y, measure = "mean", sdThreshold = 2)
  expect_true(result_y$outlier[6])
  expect_false(any(result_y$outlier[1:5]))

  # Test with NAs
  x_na <- c(1, 2, 3, NA, 5, 10, 12, 11, 9, 100)
  result_na <- physicalOutliers(x_na, na.rm = TRUE)
  expect_true(result_na$outlier[10])
  expect_true(is.na(result_na$outlier[4]))
})

test_that("moveComments moves comment columns to the end", {
  df <- data.frame(a = 1, comment1 = "text", b = 2, comment2 = "more text")
  comment_cols <- c("comment1", "comment2")

  moved_df <- moveComments(df, comment_cols)

  expect_equal(names(moved_df), c("a", "b", "comment1", "comment2"))
  expect_equal(ncol(moved_df), 4)
})

# NOTE: Tests for populateCrosswalk and convertName were not written because
# they have a hard dependency on the `deltadata::crosswalk` dataset, which
# makes them difficult to test in isolation without refactoring the functions
# to allow for a mock crosswalk to be passed.
