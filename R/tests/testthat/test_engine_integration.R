library(testthat)
source(proj_file("R", "utils.R"))
source(proj_file("R", "Engine_Wrapper.R"))

lib_path <- Sys.getenv(
  "ENGINE_LIB_PATH",
  file.path(getwd(), "build", paste0("libengine_v4", .Platform$dynlib.ext))
)

if (!file.exists(lib_path)) {
  test_that("engine library is present", {
    skip("engine library not built")
  })
} else {
  engine_load(lib_path)

  test_that("engine produces results for simple route", {
    ccys <- c("USD", "EUR", "JPY")
    routes <- build_tri_routes(ccys, "USD")
    eng <- engine_create(routes, start_ccy_index = 1L, n_ccy = length(ccys))
    on.exit({
      try(engine_stop(eng), silent = TRUE)
      rm(eng)
      gc()
    })

    expect_true(engine_start(eng))
    expect_true(engine_push_tick(eng, 1, 2, 1.10, 1.11))
    expect_true(engine_push_tick(eng, 2, 3, 1.20, 1.21))
    expect_true(engine_push_tick(eng, 3, 1, 1.30, 1.31))

    Sys.sleep(0.05)
    res <- engine_poll(eng)
    expect_s3_class(res, "data.frame")
    expect_gte(nrow(res), 1)
  })
}
