# tests/testthat/test-classification.R
test_that("bundled classification is valid", {
  pkg  <- utils::packageName()
  if (is.null(pkg)) pkg <- "rconvertu"
  path <- system.file("extdata", "classification.json", package = pkg)
  if (! nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "extdata",
                                "classification.json")
  }
  expect_true(file.exists(path), info = paste("Missing:", path))
  
  cls <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  
  expect_error(check_classification(cls), NA)   # no error
  expect_identical(check_classification(cls), TRUE)
})
