test_that("decimalDegrees converts dms to decimal degrees correctly", {
  # Test with a single value
  expect_equal(decimalDegrees("38 2 34.4", type = "dms"), 38 + 2/60 + 34.4/3600)
  # Test with longitude
  expect_equal(decimalDegrees("122 2 34.4", type = "dms", isLongitude = TRUE), -(122 + 2/60 + 34.4/3600))
  # Test with a vector
  gps_vec <- c("38 2 34.4", "39 5 10.1")
  expected_vec <- c(38 + 2/60 + 34.4/3600, 39 + 5/60 + 10.1/3600)
  expect_equal(decimalDegrees(gps_vec, type = "dms"), expected_vec)
})

test_that("decimalDegrees converts ddm to decimal degrees correctly", {
  # Test with a single value
  expect_equal(decimalDegrees("38 12.345", type = "ddm"), 38 + 12.345/60)
  # Test with longitude
  expect_equal(decimalDegrees("122 12.345", type = "ddm", isLongitude = TRUE), -(122 + 12.345/60))
})

test_that("decimalDegrees throws error for invalid type", {
  expect_error(decimalDegrees("38 12.345", type = "invalid"), "Supply type as `dms` or `ddm` only.")
})

test_that("gpsOutlier identifies outliers correctly", {
  df <- data.frame(
    date = as.Date("2023-01-01"),
    station = c("A", "A", "B", "B", "C"),
    legend = c("Theoretical", "StartTow", "Theoretical", "StartTow", "StartTow"),
    layer = "1",
    lat = c(38.0, 38.1, 40.0, 40.0, 50.0),
    lon = c(-122.0, -122.2, -120.0, -120.0, -120.0)
  )

  # Test with returnAll = FALSE (default)
  outliers <- gpsOutlier(df, d = 0.5)
  expect_equal(nrow(outliers), 2) # outlier and theoretical
  expect_equal(outliers$station[1], "A")
  expect_true(outliers$outlier[1])

  # Test with returnAll = TRUE
  all_points <- gpsOutlier(df, d = 0.5, returnAll = TRUE)
  expect_equal(nrow(all_points), 5)

  station_A_outlier <- all_points[all_points$station == "A" & all_points$legend == "StartTow", ]
  expect_true(station_A_outlier$outlier)

  station_B_outlier <- all_points[all_points$station == "B" & all_points$legend == "StartTow", ]
  expect_false(station_B_outlier$outlier)

  station_C_outlier <- all_points[all_points$station == "C" & all_points$legend == "StartTow", ]
  expect_true(is.na(station_C_outlier$outlier))
})

test_that("gpsOutlier handles missing theoretical coordinate", {
  df <- data.frame(
    date = as.Date("2023-01-01"),
    station = "A",
    legend = "StartTow",
    layer = "1",
    lat = 38.0,
    lon = -122.0
  )
  expect_error(gpsOutlier(df), "Theoretical.*must be present")
})

test_that("gpsOutlier handles missing required columns", {
  df <- data.frame(station = "A", lat = 38.0, lon = -122.0)
  expect_error(gpsOutlier(df), "Six required columns")
})

test_that("plotGPS returns a leaflet object", {
  df <- data.frame(
    date = 2023,
    station = "A",
    legend = "Theoretical",
    layer = "1",
    lat = 38.0,
    lon = -122.0
  )
  p <- plotGPS(df)
  expect_s3_class(p, "leaflet")
})
