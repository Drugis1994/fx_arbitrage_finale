library(testthat)
source(proj_file("R", "DirectStream.R"))

test_that("normalize_symbol swaps underscore and slash", {
  expect_equal(DS$normalize_symbol("EUR_USD"), "EUR/USD")
  expect_equal(DS$denorm_symbol("EUR/USD"), "EUR_USD")
})

test_that("orderbook normalization handles sparse buckets", {
  env <- environment(DS$describe)
  normalize_depth <- get(".normalize_depth", envir = env)
  normalize_orderbook <- get(".normalize_orderbook", envir = env)

  buckets <- list(
    list(price = "1.1", longCountPercent = "3", shortCountPercent = "4"),
    list(price = "1.2", longCountPercent = "5", shortCountPercent = "6")
  )

  depth <- normalize_depth(buckets, depth_limit = 5L)
  expect_equal(nrow(depth$frame), 2)
  expect_equal(depth$frame$bid_price[[1]], 1.1)

  ob <- normalize_orderbook(list(orderBook = list(buckets = buckets), time = "t1"), "EUR_USD")
  expect_equal(ob$l1$bid$price, 1.1)
  expect_equal(ob$l1$ask$price, 1.1)
  expect_equal(ob$instrument, "EUR_USD")
})
