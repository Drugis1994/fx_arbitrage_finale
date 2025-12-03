library(testthat)

test_check <- function() {
  test_dir("tests/testthat", reporter = "summary")
}

test_check()
