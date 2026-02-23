test_that("getFile handles local files", {
  tmp_file <- tempfile(fileext = ".txt")
  writeLines("test", tmp_file)
  expect_equal(getFile(tmp_file), tmp_file)
})

test_that("getFile throws error for non-existent local file", {
  expect_error(getFile("non_existent_file.txt"), "Could not find the final file")
})

test_that("architectureCheck returns NULL on non-Windows", {
  if (Sys.info()["sysname"] != "Windows") {
    expect_message(res <- architectureCheck(path32 = "somepath"), "Operating system is not Windows")
    expect_null(res)
  }
})
