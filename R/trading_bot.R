# R/Trading_Bot.R (engine-first, execution-robust)
`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============================
# Trade throttle / guards
# ==============================
.trd <- new.env(parent = emptyenv())
.trd$inflight <- FALSE
.trd$last_ts  <- as.numeric(Sys.time()) - 999
.trd$last_decision_ms <- 0

.trd_can_trade <- function(cooldown_sec) {
  now <- as.numeric(Sys.time())
  if (isTRUE(.trd$inflight)) return(FALSE)
  if ((now - .trd$last_ts) < cooldown_sec) return(FALSE)
  TRUE
}

# ==============================
# Instrument normalization
# ==============================
oanda_instr <- function(a, b) paste0(a, "_", b)
sym_to_pair <- function(sym) gsub("_", "/", sym, fixed = TRUE)
pair_to_sym <- function(pair) gsub("/", "_", pair, fixed = TRUE)

# ==============================
# State matrix helpers (for leg selection)
# ==============================
.ensure_idx_cache <- function(state) {
  if (!is.null(state$.idx)) return(invisible(state$.idx))
  ccys <- rownames(state$BID)
  idx <- seq_along(ccys); names(idx) <- ccys
  state$.idx <- idx
  invisible(idx)
}

.ij <- function(a, b, state) {
  .ensure_idx_cache(state)
  i <- state$.idx[[a]]; j <- state$.idx[[b]]
  if (is.null(i) || is.null(j)) return(c(NA_integer_, NA_integer_))
  c(as.integer(i), as.integer(j))
}

q_bid2 <- function(a, b, state) {
  ij <- .ij(a, b, state); if (any(is.na(ij))) return(NA_real_)
  v <- state$BID[ij[1], ij[2]]; if (is.finite(v)) v else NA_real_
}

q_ask2 <- function(a, b, state) {
  ij <- .ij(a, b, state); if (any(is.na(ij))) return(NA_real_)
  v <- state$ASK[ij[1], ij[2]]; if (is.finite(v)) v else NA_real_
}

# pick_leg: behold ditt valg-mønster:
# - direkte quote (a/b) -> "sell" på bid
# - fallback inverse (b/a) -> "buy" på ask
pick_leg <- function(a, b, state) {
  bq <- q_bid2(a, b, state); aq <- q_ask2(a, b, state)
  if (is.finite(bq) && is.finite(aq)) {
    return(list(mode = "direct", instrument = oanda_instr(a, b), side = "sell", quote_price = bq))
  }
  bq2 <- q_bid2(b, a, state); aq2 <- q_ask2(b, a, state)
  if (is.finite(bq2) && is.finite(aq2)) {
    return(list(mode = "inverse", instrument = oanda_instr(b, a), side = "buy", quote_price = aq2))
  }
  NULL
}

# ==============================
# OANDA response parsing (robust)
# ==============================
.num <- function(x) suppressWarnings(as.numeric(x))

.parse_oanda_market_resp <- function(resp) {
  # Expected patterns:
  # - resp$orderFillTransaction
  # - resp$orderCancelTransaction
  # - resp$orderRejectTransaction
  if (is.null(resp) || !is.list(resp)) {
    return(list(ok = FALSE, filled = FALSE, reason = "NULL_OR_NONLIST_RESPONSE"))
  }
  
  fill <- resp$orderFillTransaction %||% NULL
  if (!is.null(fill) && is.list(fill)) {
    units_signed <- .num(fill$units)
    price        <- .num(fill$price)
    instrument   <- fill$instrument %||% (resp$orderCreateTransaction$instrument %||% NA_character_)
    time         <- fill$time %||% (resp$orderCreateTransaction$time %||% NA_character_)
    txid         <- fill$id %||% (resp$lastTransactionID %||% NA_character_)
    
    if (!is.finite(units_signed) || !is.finite(price) || price <= 0) {
      return(list(ok = FALSE, filled = FALSE, reason = "BAD_FILL_NUMBERS", raw = resp))
    }
    
    return(list(
      ok = TRUE,
      filled = TRUE,
      instrument = instrument,
      time = time,
      txid = txid,
      units_signed = units_signed,
      units = abs(units_signed),
      price = price,
      raw = resp
    ))
  }
  
  cancel <- resp$orderCancelTransaction %||% resp$orderRejectTransaction %||% resp$cancelTransaction %||% NULL
  if (!is.null(cancel) && is.list(cancel)) {
    reason <- cancel$reason %||% cancel$rejectReason %||% cancel$errorMessage %||% "ORDER_CANCEL_OR_REJECT"
    txid   <- cancel$id %||% (resp$lastTransactionID %||% NA_character_)
    return(list(ok = FALSE, filled = FALSE, reason = as.character(reason), txid = txid, raw = resp))
  }
  
  list(ok = FALSE, filled = FALSE, reason = "NO_FILL_FOUND", raw = resp)
}

# ==============================
# Leg planning + execution
# ==============================
TRD_MIN_UNITS <- as.integer(getOption("arb.min_units", 1L))

.plan_leg <- function(amount_from, from_ccy, to_ccy, state) {
  leg <- pick_leg(from_ccy, to_ccy, state)
  if (is.null(leg)) return(NULL)
  
  qpx <- leg$quote_price
  if (!is.finite(qpx) || qpx <= 0) return(NULL)
  if (!is.finite(amount_from) || amount_from <= 0) return(NULL)
  
  if (identical(leg$mode, "direct")) {
    # Sell base=from_ccy units
    units_req <- as.integer(floor(amount_from))
    if (!is.finite(units_req) || units_req < TRD_MIN_UNITS) return(NULL)
    expected_recv <- units_req * qpx
  } else {
    # Buy base=to_ccy units, pay in from_ccy
    units_req <- as.integer(floor(amount_from / qpx))
    if (!is.finite(units_req) || units_req < TRD_MIN_UNITS) return(NULL)
    
    # safety: do not exceed spend
    if ((units_req * qpx) > amount_from) return(NULL)
    
    expected_recv <- units_req
  }
  
  list(
    from = from_ccy, to = to_ccy,
    mode = leg$mode,
    instrument = leg$instrument,
    side = leg$side,
    quote_price = qpx,
    units_req = units_req,
    expected_recv = expected_recv
  )
}

# Best-effort unwind: motsatt side, samme instrument, samme filled base units
.unwind_leg <- function(exec, state) {
  if (is.null(exec) || !isTRUE(exec$filled)) return(FALSE)
  
  units <- as.integer(exec$units_fill)
  if (!is.finite(units) || units <= 0) return(FALSE)
  
  side2 <- if (identical(exec$side, "buy")) "sell" else "buy"
  
  resp <- tryCatch(
    state$DS$place_market(instrument = exec$instrument, side = side2, units = units),
    error = function(e) e
  )
  if (inherits(resp, "error")) return(FALSE)
  
  ufill <- .parse_oanda_market_resp(resp)
  isTRUE(ufill$ok) && isTRUE(ufill$filled)
}

# Core: place leg and compute recv_amount using true fills
.place_leg <- function(plan, state,
                       dry_run = isTRUE(getOption("arb.dry_run", FALSE)),
                       max_slip_bps = getOption("arb.max_slip_bps", Inf)) {
  stopifnot(is.list(plan), is.environment(state))
  
  units_req <- as.integer(plan$units_req)
  if (!is.finite(units_req) || units_req <= 0) {
    return(list(ok = FALSE, filled = FALSE, reason = "BAD_UNITS_REQ", plan = plan))
  }
  
  if (isTRUE(dry_run)) {
    # Simuler "perfekt fill" på quote_price
    units_fill  <- units_req
    fill_price  <- plan$quote_price
    recv_amount <- if (identical(plan$mode, "direct")) units_fill * fill_price else units_fill
    
    return(list(
      ok = TRUE, filled = TRUE, simulated = TRUE,
      instrument = plan$instrument, side = plan$side,
      from = plan$from, to = plan$to, mode = plan$mode,
      units_req = units_req, units_fill = units_fill,
      quote_price = plan$quote_price, fill_price = fill_price,
      recv_amount = recv_amount,
      txid = NA_character_, time = as.character(Sys.time()),
      reason = NULL
    ))
  }
  
  resp <- tryCatch(
    state$DS$place_market(instrument = plan$instrument, side = plan$side, units = units_req),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(ok = FALSE, filled = FALSE, reason = conditionMessage(resp), plan = plan))
  }
  
  fill <- .parse_oanda_market_resp(resp)
  if (!isTRUE(fill$ok) || !isTRUE(fill$filled)) {
    return(c(list(plan = plan), fill))
  }
  
  units_fill <- fill$units
  fill_price <- fill$price
  
  # recv_amount always in 'to' currency
  recv_amount <- if (identical(plan$mode, "direct")) {
    units_fill * fill_price
  } else {
    units_fill
  }
  
  # Slippage guard hook (optional)
  slip_ok <- TRUE
  max_slip_bps <- suppressWarnings(as.numeric(max_slip_bps))
  if (is.finite(max_slip_bps) && max_slip_bps >= 0) {
    tol <- max_slip_bps / 1e4
    if (identical(plan$side, "sell")) {
      slip_ok <- is.finite(fill_price) && (fill_price >= plan$quote_price * (1 - tol))
    } else {
      slip_ok <- is.finite(fill_price) && (fill_price <= plan$quote_price * (1 + tol))
    }
  }
  
  if (!isTRUE(slip_ok)) {
    return(list(
      ok = FALSE, filled = TRUE, reason = "SLIPPAGE_GUARD",
      instrument = plan$instrument, side = plan$side,
      from = plan$from, to = plan$to, mode = plan$mode,
      units_req = units_req, units_fill = units_fill,
      quote_price = plan$quote_price, fill_price = fill_price,
      recv_amount = recv_amount,
      txid = fill$txid %||% NA_character_, time = fill$time %||% NA_character_,
      raw = fill$raw %||% resp,
      plan = plan
    ))
  }
  
  list(
    ok = TRUE, filled = TRUE,
    instrument = plan$instrument, side = plan$side,
    from = plan$from, to = plan$to, mode = plan$mode,
    units_req = units_req, units_fill = units_fill,
    quote_price = plan$quote_price, fill_price = fill_price,
    recv_amount = recv_amount,
    txid = fill$txid %||% NA_character_, time = fill$time %||% NA_character_,
    raw = fill$raw %||% resp
  )
}

# ================================================================
# execute_route_v4()
# Triangular arbitrage execution for engine v4 routes
# ================================================================
execute_route_v4 <- function(state, route, notional, dry_run = TRUE) {
  # ------------------------------------------------------------
  # route = one row from engine_poll():
  # route$route_id
  # route$mid
  # route$end
  # ------------------------------------------------------------
  
  CC   <- state$ccys
  S    <- CC[ state$start_i ]   # always start currency (USD)
  MID  <- CC[ route$mid ]
  END  <- CC[ route$end ]
  
  # ------------------------------------------------------------
  # Step 1 — Build instrument names (always OANDA format)
  # ------------------------------------------------------------
  pair1 <- paste0(S, "_", MID)   # S → MID (buy MID)
  pair2 <- paste0(MID, "_", END) # MID → END
  pair3 <- paste0(END, "_", S)   # END → S (sell back to start)
  
  # ------------------------------------------------------------
  # Step 2 — Fetch current prices (snapshot)
  # ------------------------------------------------------------
  px <- DS$snapshot(c(pair1, pair2, pair3))
  px1 <- px[ px$pair == gsub("_","/",pair1), ]
  px2 <- px[ px$pair == gsub("_","/",pair2), ]
  px3 <- px[ px$pair == gsub("_","/",pair3), ]
  
  if (nrow(px1)==0 || nrow(px2)==0 || nrow(px3)==0) {
    return(list(ok=FALSE, reason="missing_snapshot"))
  }
  
  # ------------------------------------------------------------
  # Step 3 — Compute theoretical fills without slippage
  # ------------------------------------------------------------
  qty_mid <- notional / px1$ask
  qty_end <- qty_mid  / px2$ask
  final   <- qty_end  * px3$bid
  
  pnl <- final - notional
  edge <- final/notional - 1
  
  # ------------------------------------------------------------
  # Reject very small or negative edges
  # ------------------------------------------------------------
  if (edge < (getOption("arb.min_edge_bps") / 1e4))
    return(list(ok=FALSE, reason="edge_too_small", pnl=pnl, edge=edge))
  
  # ------------------------------------------------------------
  # Step 4 — Liquidity gate (optional)
  # ------------------------------------------------------------
  if (isTRUE(getOption("arb.use_liquidity_gate", FALSE))) {
    # check orderbook volume on each leg
    ob1 <- DS$orderbook(pair1)
    ob2 <- DS$orderbook(pair2)
    ob3 <- DS$orderbook(pair3)
    
    if (!has_sufficient_liquidity(ob1, notional)) return(list(ok=FALSE, reason="no_liq_leg1"))
    if (!has_sufficient_liquidity(ob2, qty_mid))   return(list(ok=FALSE, reason="no_liq_leg2"))
    if (!has_sufficient_liquidity(ob3, qty_end))   return(list(ok=FALSE, reason="no_liq_leg3"))
  }
  
  # ------------------------------------------------------------
  # Step 5 — DRY RUN (simulation only)
  # ------------------------------------------------------------
  if (dry_run) {
    return(list(
      ok     = TRUE,
      dry_run= TRUE,
      S=S, MID=MID, END=END,
      pnl=pnl, edge=edge,
      theoretical_final = final,
      qty_mid=qty_mid,
      qty_end=qty_end,
      px1=px1, px2=px2, px3=px3
    ))
  }
  
  # ------------------------------------------------------------
  # Step 6 — Real execution (market orders)
  # ------------------------------------------------------------
  r1 <- DS$place_market(pair1, side="buy",  units = notional)
  if (is.null(r1)) return(list(ok=FALSE, where="leg1_fail"))
  
  r2 <- DS$place_market(pair2, side="buy",  units = qty_mid)
  if (is.null(r2)) {
    # rollback leg 1:
    DS$place_market(pair1, side="sell", units = notional)
    return(list(ok=FALSE, where="leg2_fail_rollback1"))
  }
  
  r3 <- DS$place_market(pair3, side="sell", units = qty_end)
  if (is.null(r3)) {
    # rollback leg 1 & 2:
    DS$place_market(pair2, side="sell", units = qty_mid)
    DS$place_market(pair1, side="sell", units = notional)
    return(list(ok=FALSE, where="leg3_fail_rollback12"))
  }
  
  # ------------------------------------------------------------
  # Step 7 — Success
  # ------------------------------------------------------------
  return(list(
    ok=TRUE,
    S=S, MID=MID, END=END,
    route_id = route$route_id,
    pnl=pnl,
    edge=edge,
    final=final,
    orders = list(r1=r1, r2=r2, r3=r3)
  ))
}

# ==============================
# Engine decision (poll state$eng, robust columns, low-spam)
# ==============================
.colpick <- function(df, candidates) {
  for (nm in candidates) if (nm %in% names(df)) return(nm)
  NULL
}

maybe_trade_from_engine <- function(state,
                                    start_ccy = getOption("arb.start_ccy", "USD"),
                                    bankroll  = getOption("arb.bankroll", 10000)) {
  stopifnot(is.environment(state))
  
  # Decision throttle (ms)
  min_ms <- as.integer(getOption("arb.decision_min_interval_ms", 50L))
  now_ms <- as.numeric(Sys.time()) * 1000
  if (is.finite(.trd$last_decision_ms) && (now_ms - .trd$last_decision_ms) < min_ms) {
    return(invisible(FALSE))
  }
  .trd$last_decision_ms <- now_ms
  
  df <- engine_poll(state$eng)
  if (!is.data.frame(df) || !nrow(df)) return(invisible(FALSE))
  
  mid_col  <- .colpick(df, c("mid", "mid_i", "mid_idx"))
  end_col  <- .colpick(df, c("end", "end_i", "end_idx"))
  edge_col <- .colpick(df, c("edge", "edge_before_cost", "edgeRaw"))
  pnl_col  <- .colpick(df, c("pnl", "netPnL", "net_pnl", "pnl_abs"))
  
  if (is.null(mid_col) || is.null(end_col) || is.null(edge_col)) return(invisible(FALSE))
  
  df[[mid_col]]  <- suppressWarnings(as.integer(df[[mid_col]]))
  df[[end_col]]  <- suppressWarnings(as.integer(df[[end_col]]))
  df[[edge_col]] <- suppressWarnings(as.numeric(df[[edge_col]]))
  if (!is.null(pnl_col)) df[[pnl_col]] <- suppressWarnings(as.numeric(df[[pnl_col]]))
  
  df <- df[is.finite(df[[edge_col]]), , drop = FALSE]
  if (!nrow(df)) return(invisible(FALSE))
  
  # Require both edge and pnl (optional)
  min_edge_bps <- as.numeric(getOption("arb.min_edge_bps", 5))
  min_pnl      <- as.numeric(getOption("arb.min_pnl", 0))
  require_pnl  <- isTRUE(getOption("arb.require_positive_pnl", TRUE))
  
  # pick best edge
  k <- which.max(df[[edge_col]])
  edge_bps <- 1e4 * df[[edge_col]][k]
  
  pnl_ok <- TRUE
  pnl_val <- NA_real_
  if (!is.null(pnl_col) && is.finite(df[[pnl_col]][k])) {
    pnl_val <- df[[pnl_col]][k]
    if (require_pnl) pnl_ok <- (pnl_val >= min_pnl)
  } else if (require_pnl && min_pnl > 0) {
    pnl_ok <- FALSE
  }
  
  if (!is.finite(edge_bps) || edge_bps < min_edge_bps || !isTRUE(pnl_ok)) {
    # one-line decision log (optional)
    if (isTRUE(getOption("arb.print.decisions", FALSE))) {
      cat(sprintf("DECISION skip | edge=%.2f bps | pnl=%s\n",
                  edge_bps, ifelse(is.finite(pnl_val), sprintf("%.2f", pnl_val), "NA")))
    }
    return(invisible(FALSE))
  }
  
  mid_i <- df[[mid_col]][k]
  end_i <- df[[end_col]][k]
  if (!is.finite(mid_i) || !is.finite(end_i)) return(invisible(FALSE))
  
  mid <- state$ccys[[mid_i]]
  end <- state$ccys[[end_i]]
  if (is.na(mid) || is.na(end)) return(invisible(FALSE))
  
  notional <- min(as.numeric(bankroll), as.numeric(getOption("arb.max_notional_start", 25000)))
  
  # one-line decision log
  if (isTRUE(getOption("arb.print.decisions", TRUE))) {
    cat(sprintf("DECISION trade? %s->%s->%s->%s | edge=%.2f bps | pnl=%s | notional=%.0f\n",
                start_ccy, mid, end, start_ccy,
                edge_bps,
                ifelse(is.finite(pnl_val), sprintf("%.2f", pnl_val), "NA"),
                notional))
  }
  
  res <- execute_triangle(start_ccy, mid, end, notional, state = state)
  invisible(is.list(res) && isTRUE(res$ok))
}