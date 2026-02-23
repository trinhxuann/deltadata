test_that("getTaxonomyFromItis validates input", {
  expect_error(getTaxonomyFromItis(123), "taxonNames must be a non-empty character vector")
  expect_error(getTaxonomyFromItis(character(0)), "taxonNames must be a non-empty character vector")
})
