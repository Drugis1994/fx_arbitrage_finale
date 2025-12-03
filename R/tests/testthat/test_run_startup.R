library(testthat)
Sys.setenv(FX_ARBITRAGE_TESTING = "1")

test_that("run.R can be sourced in testing mode", {
  expect_silent(withr::with_dir(project_root, suppressMessages(source(proj_file("run.R")))))
})
