library(testthat)
source(file.path("R", "utils.R"))

 test_that("build_tri_routes sorts and deduplicates", {
   routes <- build_tri_routes(c("USD", "EUR", "JPY", "EUR"), "USD")
   expect_true(is.matrix(routes))
   expect_true(all(colnames(routes) == c("mid", "end")))
   expect_gt(nrow(routes), 0)
   route_df <- attr(routes, "route_df")
   expect_true(is.data.frame(route_df))
   expect_true(all(route_df$S == match("USD", sort(unique(c("USD", "EUR", "JPY", "EUR"))))))
   expect_equal(nrow(routes), nrow(unique(route_df)))
 })
