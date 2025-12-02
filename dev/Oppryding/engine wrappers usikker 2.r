    # ================================================================
#  Engine_Wrapper.R — R KOBLING TIL engine_v4 (TRI only)
# ================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.eng <- NULL
.eng_meta <- list(ccys = NULL, start_ccy = NULL)

# -----------------------------------------------------------
# 1) Last C-motor (dylib)
# -----------------------------------------------------------
engine_load <- function(path = file.path("C++", "build", "libengine_v4.dylib")) {
  path <- normalizePath(path, mustWork = TRUE)
  if (!is.loaded("engine_create")) {
    dyn.load(path)
  }
  invisible(TRUE)
}

# -----------------------------------------------------------
# 2) Generate TRI route matrix (mid, end)
#    ✔ matches C signature EXACTLY
# -----------------------------------------------------------
precompute_route_idx <- function(ccys, start_ccy) {
  stopifnot(is.character(ccys), length(ccys) > 2L)
  
  s <- match(start_ccy, ccys)
  if (is.na(s))
    stop("precompute_route_idx: start_ccy not in ccys")
  
  idx <- setdiff(seq_along(ccys), s)
  g <- expand.grid(mid_i = idx, end_i = idx, KEEP.OUT.ATTRS = FALSE)
  g <- g[g$mid_i != g$end_i, ]
  
  out <- as.matrix(
    data.frame(
      mid_i = as.integer(g$mid_i),
      end_i = as.integer(g$end_i)
    )
  )
  storage.mode(out) <- "integer"
  out
}

.engine_push_mats <- function(ptr, state) {
  M   <- state$M
  BID <- state$BID
  ASK <- state$ASK
  
  n <- nrow(M)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      b <- BID[i, j]
      a <- ASK[i, j]
      
      if (is.finite(b) && is.finite(a) && b > 0 && a > 0) {
        .Call("engine_push_tick_R",
              ptr,
              as.integer(i),
              as.integer(j),
              as.numeric(b),
              as.numeric(a))
      }
    }
  }
  
  invisible(TRUE)
}




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
# -----------------------------------------------------------
# 3) Pump full matrix into engine
# -----------------------------------------------------------


# -----------------------------------------------------------
# 4) Init full engine (TRI only)
# -----------------------------------------------------------
engine_init <- function(state, start_ccy) {
  stopifnot(
    is.matrix(state$M),
    is.matrix(state$BID),
    is.matrix(state$ASK)
  )
  
  ccys <- rownames(state$M)
  start_i <- match(start_ccy, ccys)
  if (is.na(start_i))
    stop("engine_init: start_ccy not in M")
  
  # Build routes exactly as C expects (mid, end)
  if (is.null(state$route_idx) || !nrow(state$route_idx)) {
    state$route_idx <- precompute_route_idx(ccys, start_ccy)
  }
  
  if (!nrow(state$route_idx))
    stop("engine_init: route_idx empty")
  
  # ---------- C CALL ----------
  eng <- .Call(
    "engine_create",
    state$route_idx,           # integer matrix (n x 2)
    as.integer(start_i),       # start index (1-based)
    as.integer(length(ccys))   # n_ccy
  )
  # -----------------------------
  
  # Start C-thread
  .Call("engine_start_R", eng)
  
  # Push initial matrix state
  .engine_push_mats(eng, state)
  
  # Save static pointer
  .eng <<- eng
  .eng_meta <<- list(
    ccys = ccys,
    start_ccy = start_ccy
  )
  
  invisible(TRUE)
}

# -----------------------------------------------------------
# 5) Refresh engine matrix (on rebuild)
# -----------------------------------------------------------
engine_refresh_mats <- function(state) {
  if (is.null(.eng)) return(invisible(FALSE))
  .engine_push_mats(.eng, state)
  invisible(TRUE)
}

engine_reinit_if_needed <- function(state, start_ccy) {
  ccys <- rownames(state$M)
  
  need <- (
    is.null(.eng) ||
      !identical(.eng_meta$ccys, ccys) ||
      !identical(.eng_meta$start_ccy, start_ccy)
  )
  
  if (need)
    engine_init(state, start_ccy)
  
  invisible(TRUE)
}

# -----------------------------------------------------------
# 6) Poll → data.frame
# -----------------------------------------------------------
engine_poll_df <- function() {
  if (is.null(.eng)) return(NULL)
  
  df <- .Call("engine_poll", .eng)
  if (is.null(df) || nrow(df) == 0)
    return(NULL)
  
  df
}

# -----------------------------------------------------------
# 7) Tick eval (just poll)
# -----------------------------------------------------------
engine_tick_eval <- function(state) {
  if (is.null(.eng)) return(NULL)
  engine_poll_df()
}

engine_wrappers_ready <- function() TRUE




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










