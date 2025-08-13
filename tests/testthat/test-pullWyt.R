test_that("wytDate returns correct water year type and index", {
  wyt_df <- data.frame(
    waterYear = c(2022, 2023, 2024),
    sacIndex = c(5.0, 9.0, 10.0),
    sacWyt = c("D", "W", "AN"),
    sjrIndex = c(2.0, 4.0, 5.0),
    sjrWyt = c("C", "D", "BN")
  )

  # Test case 1: Single date, default valley ("sac") and value ("wyt")
  expect_equal(wytDate(as.Date("2023-05-15"), wyt = wyt_df), c("2023" = "W"))

  # Test case 2: Water year boundary (September vs October)
  expect_equal(wytDate(as.Date("2023-09-30"), wyt = wyt_df), c("2023" = "W"))
  expect_equal(wytDate(as.Date("2023-10-01"), wyt = wyt_df), c("2024" = "AN"))

  # Test case 3: Vector of dates
  dates <- as.Date(c("2022-11-01", "2023-04-01", "2023-12-25"))
  expected <- c("2023" = "W", "2023" = "W", "2024" = "AN")
  expect_equal(wytDate(dates, wyt = wyt_df), expected)

  # Test case 4: Different valley ("sjr")
  expect_equal(wytDate(as.Date("2023-05-15"), wyt = wyt_df, valley = "sjr"), c("2023" = "D"))

  # Test case 5: Different value ("index")
  expect_equal(wytDate(as.Date("2023-05-15"), wyt = wyt_df, value = "index"), c("2023" = 9.0))

  # Test case 6: Combination of valley and value
  expect_equal(wytDate(as.Date("2023-05-15"), wyt = wyt_df, valley = "sjr", value = "index"), c("2023" = 4.0))

  # Test case 7: Date not in the table
  expect_equal(wytDate(as.Date("2021-01-01"), wyt = wyt_df), c("2021" = NA_character_))
})

test_that("wytDate handles argument matching", {
  wyt_df <- data.frame(
    waterYear = 2023, sacWyt = "W", sjrWyt = "D", sacIndex = 9.0, sjrIndex = 4.0
  )
  expect_error(wytDate(as.Date("2023-01-01"), wyt_df, valley = "invalid"))
  expect_error(wytDate(as.Date("2023-01-01"), wyt_df, value = "invalid"))
})
