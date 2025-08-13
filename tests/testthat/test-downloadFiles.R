test_that("parseEDI correctly parses EDI URLs", {
  # Test case 1: URL with scope, identifier, and revision
  url1 <- "https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=534&revision=8"
  expected1 <- data.frame(scope = "edi", identifier = "534", revision = "8", packageID = "edi.534.8")
  expect_equal(parseEDI(url1), expected1)

  # Test case 2: URL with packageid
  url2 <- "https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.8"
  expected2 <- data.frame(scope = "edi", identifier = "534", revision = 8, packageID = "edi.534.8")
  expect_equal(parseEDI(url2), expected2)

  # Test case 3: URL with scope and identifier, but no revision (should default to "1")
  url3 <- "https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=534"
  expected3 <- data.frame(scope = "edi", identifier = "534", revision = "1", packageID = "edi.534.1")
  expect_equal(parseEDI(url3), expected3)

  # Test case 4: another packageid example with different structure
  url4 <- "https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.1.27"
  expected4 <- data.frame(scope = "knb-lter-sbc", identifier = "1", revision = 27, packageID = "knb-lter-sbc.1.27")
  expect_equal(parseEDI(url4), expected4)
})
