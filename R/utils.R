# Shared utilities for portable runtime + routing

`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_root <- function() {
  env_root <- Sys.getenv("FX_ARBITRAGE_ROOT", unset = NA_character_)
  if (!is.na(env_root) && nzchar(env_root)) {
    return(normalizePath(env_root, mustWork = FALSE))
  }

  # Prefer the directory containing this file when sourced
  this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(...) NA_character_)
  if (!is.na(this_file)) {
    return(normalizePath(file.path(dirname(this_file), ".."), mustWork = FALSE))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

load_env_files <- function(root) {
  env_path <- file.path(root, ".env")
  if (requireNamespace("dotenv", quietly = TRUE) && file.exists(env_path)) {
    dotenv::load_dot_env(env_path, override = FALSE)
  } else if (file.exists(env_path)) {
    readRenviron(env_path)
  }
  invisible(TRUE)
}

build_tri_routes <- function(ccys, start_ccy) {
  stopifnot(is.character(ccys), length(ccys) >= 3)
  start_idx <- match(start_ccy, ccys)
  if (is.na(start_idx)) stop("start_ccy not present in currency universe")

  others <- setdiff(seq_along(ccys), start_idx)
  if (length(others) < 2) stop("Need at least two non-start currencies for a triangle")

  combos <- expand.grid(mid = others, end = others, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  combos <- combos[combos$mid != combos$end, , drop = FALSE]
  combos$route_id <- seq_len(nrow(combos))
  combos$S <- start_idx
  combos$X <- combos$mid
  combos$Y <- combos$end

  routes <- as.matrix(combos[, c("mid", "end")])
  storage.mode(routes) <- "integer"
  colnames(routes) <- c("mid", "end")

  attr(routes, "route_df") <- combos[, c("route_id", "S", "X", "Y")]
  routes
}

route_callgraph <- function() {
  data.frame(
    step = c("engine_init", "engine_start", "engine_push_tick", "eval_all_routes (C++)", "engine_poll"),
    R_to_C = c("Engine_Wrapper.R", "Engine_Wrapper.R", "Engine_Wrapper.R", "engine.c", "engine_R.c"),
    C_to_R = c("engine_create -> engine_start_R", "engine_start_R", "engine_push_tick_R", "result_ring -> engine_poll", "Engine_Wrapper.R"),
    stringsAsFactors = FALSE
  )
}
