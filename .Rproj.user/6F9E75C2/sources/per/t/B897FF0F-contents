# ========== Engine wrappers (felles) ==========
.eng <- NULL
.eng_meta <- list(ccys = NULL, start_ccy = NULL)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------
# Local route-index builder
# ---------------------------
# Holder wrapperen selvstendig og uavhengig av load-rekkefølge.
precompute_route_idx <- function(ccys, start_ccy) {
  stopifnot(is.character(ccys), length(ccys) > 2L)
  s <- match(start_ccy, ccys)
  if (is.na(s)) stop("precompute_route_idx: start_ccy not in ccys", call. = FALSE)
  idx <- setdiff(seq_along(ccys), s)
  g <- expand.grid(mid_i = idx, end_i = idx, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  g <- subset(g, mid_i != end_i)
  as.matrix(data.frame(mid_i = as.integer(g$mid_i), end_i = as.integer(g$end_i)))
}

# Toleranse fra options (definert i run.R)
.ENG_TOL <- function() as.numeric(getOption("arb.matrix_tol", 1e-12))

# ---------------------------
# Engine lifecycle + helpers
# ---------------------------

engine_init <- function(state, start_ccy) {
  stopifnot(is.matrix(state$M), is.matrix(state$BID), is.matrix(state$ASK))
  ccys <- rownames(state$M); s_i <- match(start_ccy, ccys)
  if (is.na(s_i)) stop("engine_init: start_ccy ikke i M")
  
  if (is.null(state$route_idx) || !nrow(state$route_idx)) {
    state$route_idx <- precompute_route_idx(ccys, start_ccy)
  }
  if (!nrow(state$route_idx)) stop("engine_init: tom route_idx")
  
  xp <- engine_create(state$route_idx, s_i, length(ccys))
  # Viktig: push matriser OG marker endringer i én operasjon
  engine_set_matrices_and_mark_changes(xp, state$M, state$BID, state$ASK, tol = .ENG_TOL())
  engine_rebuild_participation(xp)
  
  .eng <<- xp
  .eng_meta <<- list(ccys = ccys, start_ccy = start_ccy)
  invisible(TRUE)
}

engine_refresh_mats <- function(state) {
  if (is.null(.eng)) return(invisible(FALSE))
  # Hold skitten-markering oppdatert hver gang vi pusher matriser
  engine_set_matrices_and_mark_changes(.eng, state$M, state$BID, state$ASK, tol = .ENG_TOL())
  invisible(TRUE)
}

engine_mark_m_changes <- function(prevM, newM, tol = .ENG_TOL()) {
  # Beholdes for API-kompatibilitet; ikke nødvendig når vi bruker
  # engine_set_matrices_and_mark_changes(). No-op:
  invisible(TRUE)
}

engine_eval_topK <- function(K,
                             fee_pct,
                             slip_base_frac,
                             buf_base_frac,
                             k_slip,
                             rvbps_fallback_bps,
                             start_qty_base) {
  if (is.null(.eng)) return(NULL)
  engine_eval_dirty(.eng,
                    as.integer(K),
                    as.numeric(fee_pct),
                    as.numeric(slip_base_frac),
                    as.numeric(buf_base_frac),
                    as.numeric(k_slip),
                    as.numeric(rvbps_fallback_bps),
                    as.numeric(start_qty_base))
}

# Automatisk re-init når universet eller start-ccy endres (feature-flag)
engine_reinit_if_needed <- function(state, start_ccy) {
  ccys <- rownames(state$M)
  need <- is.null(.eng) ||
    isTRUE(getOption("arb.recompute_routes_on_ccy_change", TRUE)) &&
    (!identical(.eng_meta$ccys, ccys) || !identical(.eng_meta$start_ccy, start_ccy))
  if (need) engine_init(state, start_ccy)
  invisible(TRUE)
}

# Hjelper som brukes i on_tick/evaluate
engine_tick_eval <- function(state,
                             K_print,
                             start_qty_base,
                             fee_pct,
                             assumed_slip_bps,
                             safety_buffer_bps,
                             k_slip = getOption("arb.k_slip", 0.30),
                             rvbps_fallback_bps = getOption("arb.rvbps_fallback_bps", 10.0)) {
  slip_base_frac <- (assumed_slip_bps %||% 0) / 1e4
  buf_base_frac  <- (safety_buffer_bps %||% 0) / 1e4
  res <- engine_eval_topK(
    K = max(1L, K_print),
    fee_pct = fee_pct,
    slip_base_frac = slip_base_frac,
    buf_base_frac  = buf_base_frac,
    k_slip = k_slip,
    rvbps_fallback_bps = rvbps_fallback_bps,
    start_qty_base = start_qty_base
  )
  if (is.null(res) || !length(res$idx)) return(NULL)
  as.data.frame(res, stringsAsFactors = FALSE)
}

# -----------------------------------------
# Pure-R fallbacks expected by evaluate_all
# -----------------------------------------

# edge_screen_cpp: mids-only edge screen (R fallback)
# Returns columns: mid_i, end_i, dmid, vmid, mmid, edge
if (!exists("edge_screen_cpp", mode = "function")) {
  edge_screen_cpp <- function(s_i, route_idx, M) {
    stopifnot(is.matrix(M), is.matrix(route_idx), ncol(route_idx) >= 2L)
    mid_i <- as.integer(route_idx[, 1])
    end_i <- as.integer(route_idx[, 2])
    
    if (!length(mid_i)) {
      return(data.frame(
        mid_i = integer(), end_i = integer(),
        dmid = numeric(), vmid = numeric(), mmid = numeric(),
        edge = numeric(), stringsAsFactors = FALSE
      ))
    }
    
    vmid <- M[s_i, mid_i]
    mmid <- M[mid_i, end_i]
    dmid <- M[s_i, end_i]
    
    edge <- rep(NA_real_, length(mid_i))
    ok <- is.finite(vmid) & is.finite(mmid) & is.finite(dmid) & (dmid > 0)
    edge[ok] <- (vmid[ok] * mmid[ok]) / dmid[ok] - 1
    
    data.frame(
      mid_i = mid_i,
      end_i = end_i,
      dmid = dmid,
      vmid = vmid,
      mmid = mmid,
      edge = edge,
      stringsAsFactors = FALSE
    )
  }
}

# tri_pnl_cpp: three-leg PnL using BID/ASK with per-leg fee (R fallback)
# Returns: ok, final_base, netPnL, pnl_pct, leg1_side, leg2_side, leg3_side
if (!exists("tri_pnl_cpp", mode = "function")) {
  tri_pnl_cpp <- function(start_i, route_idx, BID, ASK, start_qty_base, fee_pct) {
    stopifnot(
      is.matrix(BID), is.matrix(ASK),
      nrow(BID) == ncol(BID), nrow(ASK) == ncol(ASK),
      is.matrix(route_idx), ncol(route_idx) >= 2L
    )
    if (nrow(route_idx) < 1L) {
      return(data.frame(
        ok = FALSE, final_base = NA_real_, netPnL = NA_real_, pnl_pct = NA_real_,
        leg1_side = NA_integer_, leg2_side = NA_integer_, leg3_side = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    
    mid_i <- as.integer(route_idx[1, 1]); end_i <- as.integer(route_idx[1, 2])
    
    convert_qty <- function(qty, from_i, to_i, BID, ASK, fee) {
      # Prefer direct BID[from,to] (sell FROM to buy TO).
      rate_bid <- BID[from_i, to_i]
      if (is.finite(rate_bid)) {
        qty_to <- (qty - fee * qty) * rate_bid
        return(list(qty = qty_to, side = 1L))  # 1 = bid/direct
      }
      # Fallback: inverse via ASK[to,from]
      rate_ask_inv <- ASK[to_i, from_i]
      if (is.finite(rate_ask_inv)) {
        qty_to <- (qty - fee * qty) / rate_ask_inv
        return(list(qty = qty_to, side = 0L))  # 0 = ask/inverse
      }
      list(qty = NA_real_, side = NA_integer_)
    }
    
    q1 <- convert_qty(start_qty_base, start_i, mid_i, BID, ASK, fee_pct)
    if (!is.finite(q1$qty)) {
      return(data.frame(
        ok = FALSE, final_base = NA_real_, netPnL = NA_real_, pnl_pct = NA_real_,
        leg1_side = NA_integer_, leg2_side = NA_integer_, leg3_side = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    q2 <- convert_qty(q1$qty, mid_i, end_i, BID, ASK, fee_pct)
    if (!is.finite(q2$qty)) {
      return(data.frame(
        ok = FALSE, final_base = NA_real_, netPnL = NA_real_, pnl_pct = NA_real_,
        leg1_side = q1$side, leg2_side = NA_integer_, leg3_side = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    q3 <- convert_qty(q2$qty, end_i, start_i, BID, ASK, fee_pct)
    if (!is.finite(q3$qty)) {
      return(data.frame(
        ok = FALSE, final_base = NA_real_, netPnL = NA_real_, pnl_pct = NA_real_,
        leg1_side = q1$side, leg2_side = q2$side, leg3_side = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    
    final_base <- q3$qty
    netPnL     <- final_base - start_qty_base
    pnl_pct    <- if (is.finite(final_base)) (netPnL / start_qty_base) * 100 else NA_real_
    
    data.frame(
      ok = TRUE,
      final_base = final_base,
      netPnL = netPnL,
      pnl_pct = pnl_pct,
      leg1_side = q1$side,
      leg2_side = q2$side,
      leg3_side = q3$side,
      stringsAsFactors = FALSE
    )
  }
}

# ---- Single-triangle PnL wrapper (sanity) -----------------------------------
tri_arbitrage_pnl <- function(start_ccy, mid_ccy, end_ccy,
                              state = .state,
                              start_qty_base = 100) {
  # Sørg for at M/BID/ASK/ccys finnes
  if (!is.matrix(state$M) || !is.matrix(state$BID) || !is.matrix(state$ASK)) {
    if (exists("ensure_M", mode = "function")) ensure_M(state)
  }
  stopifnot(is.matrix(state$BID), is.matrix(state$ASK))
  ccys <- rownames(state$BID)
  
  s_i   <- match(start_ccy, ccys)
  mid_i <- match(mid_ccy,   ccys)
  end_i <- match(end_ccy,   ccys)
  if (any(is.na(c(s_i, mid_i, end_i)))) {
    stop("tri_arbitrage_pnl: en eller flere valutaer finnes ikke i matrisene")
  }
  
  # Én rute: (start -> mid -> end -> start)
  route_idx <- cbind(mid_i, end_i)
  fee_pct   <- getOption("arb.fee_pct", 0)
  
  res <- try(tri_pnl_cpp(
    start_i        = s_i,
    route_idx      = route_idx,
    BID            = state$BID,
    ASK            = state$ASK,
    start_qty_base = start_qty_base,
    fee_pct        = fee_pct
  ), silent = TRUE)
  
  if (inherits(res, "try-error") || !is.data.frame(res) || !nrow(res)) {
    return(list(ok = FALSE, err = "tri_pnl_cpp_failed"))
  }
  
  list(
    ok               = isTRUE(res$ok[1]),
    final_base       = res$final_base[1],
    netPnL           = res$netPnL[1],
    pnl_after_costs  = res$netPnL[1],  # fee er allerede trukket i tri_pnl_cpp
    pnl_pct          = res$pnl_pct[1],
    leg1_side        = res$leg1_side[1],
    leg2_side        = res$leg2_side[1],
    leg3_side        = res$leg3_side[1]
  )
}

engine_wrappers_ready <- function() {
  all(c("edge_screen_cpp","tri_pnl_cpp") %in% ls(envir = .GlobalEnv))
}











  



