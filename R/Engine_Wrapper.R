# ============================================================
#  ENGINE WRAPPER FOR ENGINE v4 (TRI + CYCLE)
#  Komplett, korrekt og matchet til C-koden du ga meg
# ============================================================

# -----------------------------
# Load the shared library
# -----------------------------
engine_load <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  dyn.load(path)
  invisible(TRUE)
}



# -----------------------------
# Create engine (TRI + CYCLE)
# -----------------------------
engine_create <- function(routes_matrix, start_ccy_index, n_ccy) {
  stopifnot(is.matrix(routes_matrix))
  stopifnot(is.integer(routes_matrix))
  stopifnot(length(start_ccy_index) == 1L)
  stopifnot(length(n_ccy) == 1L)
  
  .Call(
    "engine_create",
    routes_matrix,
    as.integer(start_ccy_index),
    as.integer(n_ccy)
  )
}

# -----------------------------
# Start engine thread
# -----------------------------
engine_start <- function(ptr) {
  .Call("engine_start_R", ptr)
}


  
# -----------------------------
# Stop engine thread
# -----------------------------
engine_stop <- function(ptr) {
  .Call("engine_stop_R", ptr)
}

# -----------------------------
# Push a single tick
# -----------------------------
engine_push_tick <- function(ptr, i, j, bid, ask) {
  .Call(
    "engine_push_tick_R",
    ptr,
    as.integer(i),
    as.integer(j),
    as.numeric(bid),
    as.numeric(ask)
  )
}




# -----------------------------
# Push full BID/ASK matrix
# -----------------------------
engine_push_full <- function(ptr, M, BID, ASK) {
  n <- nrow(M)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      b <- BID[i, j]
      a <- ASK[i, j]
      if (is.finite(b) && is.finite(a) && b > 0 && a > 0) {
        engine_push_tick(ptr, i, j, b, a)
      }
    }
  }
  invisible(TRUE)
}

# -----------------------------
# Poll all results
# -----------------------------
engine_poll <- function(ptr) {
  .Call("engine_poll", ptr)
}


# -----------------------------
# Full initializer
# -----------------------------
engine_init <- function(state, start_ccy) {
  start_i <- match(start_ccy, state$ccys)
  if (is.na(start_i)) stop("start_ccy not in state$ccys")

  # Build TRI routes (S=start_ccy, X=mid, Y=end)
  routes <- build_tri_routes(state$ccys, start_ccy)
  state$routes <- routes
  state$routes_df <- attr(routes, "route_df")

  # Create engine in C
  state$eng <- engine_create(
    routes_matrix = routes,
    start_ccy_index = start_i,
    n_ccy = length(state$ccys)
  )

  # Fill matrix
  engine_push_full(state$eng, state$M, state$BID, state$ASK)

  # Start computation thread
  engine_start(state$eng)

  state
}


ns <- function() {
  # time in nanoseconds as integer64
  as.integer64(Sys.time())  # high-resolution
}










