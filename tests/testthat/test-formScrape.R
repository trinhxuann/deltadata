test_that("uniqueNames handles vectors with no duplicates", {
  x <- c("a", "b", "c")
  result <- uniqueNames(x)
  expect_equal(result$uniqueNames, c("a", "b", "c"))
  expect_equal(result$firstIndex, character(0))
})

test_that("uniqueNames handles vectors with one set of duplicates", {
  x <- c("a", "b", "a", "c")
  result <- uniqueNames(x)
  expect_equal(result$uniqueNames, c("a", "b", "a_2", "c"))
  expect_equal(result$firstIndex, "a_2")
})

test_that("uniqueNames handles multiple sets of duplicates", {
  x <- c("a", "b", "a", "b", "c")
  result <- uniqueNames(x)
  expect_equal(result$uniqueNames, c("a", "b", "a_2", "b_2", "c"))
  # The original code returns only the first element of the firstIndex.
  expect_equal(result$firstIndex, "a_2")
})

test_that("uniqueNames fails with more than two duplicates of one element", {
  # This test exposes a bug in the original function where it tries to subset a vector as a matrix.
  x <- c("a", "a", "a", "b")
  expect_error(uniqueNames(x))
})
