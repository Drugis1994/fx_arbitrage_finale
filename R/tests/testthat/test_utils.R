library(testthat)
source(proj_file("R", "utils.R"))

test_that("resolve_root respects environment variable", {
  old <- Sys.getenv("FX_ARBITRAGE_ROOT")
  on.exit(Sys.setenv(FX_ARBITRAGE_ROOT = old))
  Sys.setenv(FX_ARBITRAGE_ROOT = tempfile("arb_root_"))
  expect_true(grepl("arb_root_", resolve_root()))
})

test_that("build_tri_routes returns expected route matrix", {
  ccys <- c("USD", "EUR", "JPY")
  routes <- build_tri_routes(ccys, "USD")
  expect_type(routes, "integer")
  expect_equal(dim(routes), c(2, 2))
  expect_true(all(routes[, 1] != routes[, 2]))
  meta <- attr(routes, "route_df")
  expect_s3_class(meta, "data.frame")
  expect_equal(nrow(meta), 2)
})
