library(testthat)
source(file.path("R", "DirectStream.R"))

# Access internal normalizer without performing HTTP
normalize_orderbook <- get(".normalize_orderbook", envir = environment(DS$orderbook), inherits = TRUE)

test_that("orderbook exposes normalize argument for compatibility", {
  args <- names(formals(DS$orderbook))
  expect_true("normalize" %in% args)
})

test_that("normalize_orderbook builds l1/l2 fields", {
  raw <- list(
    time = "2024-01-01T00:00:00Z",
    orderBook = list(buckets = list(list(price = 1.1, longCountPercent = 2, shortCountPercent = 3)))
  )
  ob <- normalize_orderbook(raw, "EUR_USD")
  expect_equal(ob$instrument, "EUR_USD")
  expect_true(is.list(ob$l1))
  expect_true(is.list(ob$l2))
  expect_equal(ob$l1$bid$price, 1.1)
})
