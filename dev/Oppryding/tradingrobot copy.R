# R/trading_bot.R (C++ engine-first, clean)
# -----------------------------------------
# Trading bot with:
#  - Dynamic slippage from recent volatility (config via options)
#  - Sequential 3-leg execution with rollback/hedge attempts
#  - Fractional Kelly sizing (live via options)
#  - Error resilience + spread/liquidity gating
#  - Engine-first route screening (dirty-routing C++)
#  - ALL prints routed through arb.core_logger
# -----------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# ===== Live config accessor (reads options each call) =========================
TRD_CFG <- function() {
  list(
    cooldown_sec = as.numeric(getOption("arb.cooldown_sec",        0.0)),
    dry_run      = isTRUE(getOption("arb.dry_run",                 TRUE)),
    frac_kelly   = as.numeric(getOption("arb.frac_kelly",          2e-04)),
    max_notional = as.numeric(getOption("arb.max_notional_start",  25000)),
    min_edge_bps = as.integer(getOption("arb.min_edge_bps",        5L)),
    min_units    = as.integer(getOption("arb.min_units",           1L))
  )
}

# --- ensure_M: rebuild matrices from state$latest (idempotent) ---------------
ensure_M <- function(state = .state) {
  stopifnot(is.environment(state), exists("latest", envir = state, inherits = FALSE))
  qm <- build_quote_map(as.list.environment(state$latest))
  if (!length(ls(qm))) stop("No quotes in state$latest", call. = FALSE)
  pm <- make_price_matrices(qm)
  stopifnot(is.matrix(pm$M), nrow(pm$M) > 0)
  state$qm   <- qm
  state$M    <- pm$M
  state$BID  <- pm$BID
  state$ASK  <- pm$ASK
  new_ccys   <- rownames(pm$M)
  if (is.null(state$ccys) || !identical(state$ccys, new_ccys)) {
    state$ccys <- new_ccys
    state$.idx <- NULL
  } else {
    state$ccys <- new_ccys
  }
  return(state$M)
}

# ===== quotes (matrix-first) ==================================================
q_bid <- function(pair, state = .state) {
  x <- state$latest[[pair]]; if (is.null(x)) return(NA_real_); as.numeric(x$bid)
}
q_ask <- function(pair, state = .state) {
  x <- state$latest[[pair]]; if (is.null(x)) return(NA_real_); as.numeric(x$ask)
}
q_mid <- function(pair, state = .state) {
  b <- q_bid(pair, state); a <- q_ask(pair, state)
  if (!is.finite(b) || !is.finite(a)) return(NA_real_)
  0.5*(a + b)
}

.ensure_idx_cache <- function(state = .state) {
  if (!is.null(state$.idx)) return(invisible(state$.idx))
  ccys <- state$ccys %||% rownames(state$BID)
  stopifnot(is.character(ccys), length(ccys) > 0)
  idx <- seq_along(ccys); names(idx) <- ccys
  state$.idx <- idx
  invisible(idx)
}

.ij <- function(a, b, state = .state) {
  idx <- .ensure_idx_cache(state) %||% state$.idx
  i <- idx[[a]]; j <- idx[[b]]
  if (is.null(i) || is.null(j)) return(c(NA_integer_, NA_integer_))
  c(as.integer(i), as.integer(j))
}

q_bid2 <- function(a, b, state = .state) {
  if (!is.null(state$BID)) {
    ij <- .ij(a, b, state); if (any(is.na(ij))) return(NA_real_)
    v <- state$BID[ij[1], ij[2]]; if (is.finite(v)) return(v)
  }
  q_bid(paste0(a,"/",b), state)
}

q_ask2 <- function(a, b, state = .state) {
  if (!is.null(state$ASK)) {
    ij <- .ij(a, b, state); if (any(is.na(ij))) return(NA_real_)
    v <- state$ASK[ij[1], ij[2]]; if (is.finite(v)) return(v)
  }
  q_ask(paste0(a,"/",b), state)
}

# ===== dynamic slippage (vol) =================================================
.vol_env <- new.env(parent = emptyenv())

push_mid <- function(pair, mid, max_n = getOption("arb.vol.buf_len", 120L)) {
  if (!is.finite(mid)) return(invisible())
  rb <- .vol_env[[pair]]
  if (is.null(rb)) rb <- list(buf = numeric(max_n), idx = 0L, n = 0L)
  if (length(rb$buf) != max_n) rb$buf <- numeric(max_n)
  idx <- (rb$idx %% max_n) + 1L
  rb$buf[idx] <- mid
  rb$idx <- idx
  rb$n <- min(rb$n + 1L, max_n)
  .vol_env[[pair]] <- rb
}

vol_bps <- function(pair, window = getOption("arb.vol.window", 60L)) {
  rb <- .vol_env[[pair]]; if (is.null(rb) || rb$n < 3L) return(NA_real_)
  n <- min(window, rb$n)
  out <- numeric(n)
  for (k in 1:n) {
    pos <- ((rb$idx - (n - k)) - 1L) %% length(rb$buf) + 1L
    out[k] <- rb$buf[pos]
  }
  sd(diff(log(out)), na.rm = TRUE) * 1e4
}

# Aksepterer ... slik at gamle kall med state=state ikke feiler
route_vol_bps <- function(pairs, ...) {
  vals <- vapply(pairs, vol_bps, numeric(1))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(NA_real_)
  max(vals)
}

# ===== spread caps + gating ===================================================
.init_cap_matrix <- function(state = .state) {
  if (!is.null(state$CAP)) return(invisible(state$CAP))
  ccys <- state$ccys %||% rownames(state$BID)
  .ensure_idx_cache(state)
  n <- length(ccys)
  CAP <- matrix(getOption("arb.spread_default_cap_bps", 20.0),
                n, n, dimnames = list(ccys, ccys))
  spread_caps <- getOption("arb.spread_cap")
  for (a in ccys) for (b in ccys) {
    if (a == b) next
    cap_from <- tryCatch(spread_caps[[a]], error = function(e) NA_real_)
    cap_to   <- tryCatch(spread_caps[[b]], error = function(e) NA_real_)
    cap <- suppressWarnings(max(c(cap_from, cap_to), na.rm = TRUE))
    if (is.finite(cap)) CAP[a, b] <- as.numeric(cap)
  }
  state$CAP <- CAP
  invisible(CAP)
}

max_spread_for2 <- function(a, b, state = .state) {
  .init_cap_matrix(state)
  ij <- .ij(a, b, state); if (any(is.na(ij))) return(Inf)
  state$CAP[ij[1], ij[2]]
}

pair_spread_bps2 <- function(a, b, state = .state) {
  bq <- q_bid2(a, b, state); aq <- q_ask2(a, b, state)
  if (!is.finite(bq) || !is.finite(aq) || bq <= 0 || aq <= 0) return(Inf)
  m   <- 0.5 * (aq + bq)
  bps <- (aq - bq) / m * 1e4
  cap <- max_spread_for2(a, b, state)
  if (bps > cap) Inf else bps
}

# ===== liquidity (lightweight gates) =========================================
has_liquidity <- function(from_ccy, to_ccy,
                          min_notional_from = getOption("arb.liq.min_notional", 20),
                          state = .state,
                          depth_levels = getOption("arb.liq.depth_levels", 3L),
                          ttl_ms = getOption("arb.liq.cache_ttl_ms", 300L)) {
  # Vi må støtte at Oanda ikke tilbyr alle kryss i "direkte" retning.
  # Prøv både FROM_TO og TO_FROM og aksepter likviditet hvis én av de finnes.
  direct  <- paste0(from_ccy, "_", to_ccy)
  inverse <- paste0(to_ccy, "_", from_ccy)
  
  now_ms <- as.numeric(Sys.time()) * 1000
  
  # cache-hit (direkte)
  oc <- state$ob_cache[[direct]]
  if (!is.null(oc) && (now_ms - oc$ts) < ttl_ms) {
    if (isTRUE(oc$ok)) return(TRUE)
    # hvis direkte var FALSE, prøv inverse-cache før vi går til API
    oc2 <- state$ob_cache[[inverse]]
    if (!is.null(oc2) && (now_ms - oc2$ts) < ttl_ms) return(isTRUE(oc2$ok))
  }
  
  orderbook_ok <- function(ob) {
    if (inherits(ob, "try-error") || is.null(ob)) return(FALSE)
    if (!is.null(ob$price) && length(ob$price)) return(TRUE)
    if (!is.null(ob$orderBook)) {
      if (!is.null(ob$orderBook$price) && length(ob$orderBook$price)) return(TRUE)
      if (!is.null(ob$orderBook$buckets) && NROW(ob$orderBook$buckets)) return(TRUE)
    }
    FALSE
  }
  
  ok_direct <- FALSE
  ok_inverse <- FALSE
  
  # prøv direkte
  ob1 <- try(state$DS$orderbook(direct), silent = TRUE)
  ok_direct <- orderbook_ok(ob1)
  state$ob_cache[[direct]] <- list(ts = now_ms, ok = ok_direct)
  
  if (!ok_direct) {
    # prøv inverse
    ob2 <- try(state$DS$orderbook(inverse), silent = TRUE)
    ok_inverse <- orderbook_ok(ob2)
    state$ob_cache[[inverse]] <- list(ts = now_ms, ok = ok_inverse)
  }
  
  isTRUE(ok_direct || ok_inverse)
}

# ===== choose instrument/side + units ========================================
pick_leg <- function(a, b, state = .state) {
  # Meglerens symbols (kan feile – da faller vi tilbake til gammel logikk)
  syms <- try(state$DS$list_symbols(), silent = TRUE)
  listed <- if (!inherits(syms, "try-error")) as.character(syms) else NULL
  
  ab  <- paste0(a,"_",b)
  ba  <- paste0(b,"_",a)
  bq1 <- q_bid2(a,b,state); aq1 <- q_ask2(a,b,state)
  bq2 <- q_bid2(b,a,state); aq2 <- q_ask2(b,a,state)
  
  # 1) Foretrekk retningen som faktisk er LISTET hos megler + har quotes
  if (!is.null(listed) && (ab %in% listed) && is.finite(bq1) && is.finite(aq1))
    return(list(instrument = ab, side = "sell", price = bq1, pair = paste0(a,"/",b)))
  if (!is.null(listed) && (ba %in% listed) && is.finite(bq2) && is.finite(aq2))
    return(list(instrument = ba, side = "buy",  price = aq2, pair = paste0(b,"/",a)))
  
  # 2) Fallback: gammel logikk hvis vi mangler listing
  if (is.finite(bq1) && is.finite(aq1))
    return(list(instrument = ab, side = "sell", price = bq1, pair = paste0(a,"/",b)))
  if (is.finite(bq2) && is.finite(aq2))
    return(list(instrument = ba, side = "buy",  price = aq2, pair = paste0(b,"/",a)))
  
  NULL
}

amount_to_units <- function(amount_in_A, from_ccy, to_ccy, state = .state) {
  cfg <- TRD_CFG()
  leg <- pick_leg(from_ccy, to_ccy, state); if (is.null(leg)) return(NULL)
  if (leg$side == "sell") {
    units_base <- amount_in_A
    units <- max(cfg$min_units, floor(units_base))
    recv_B <- units * leg$price
    list(instrument = leg$instrument, side = "sell", units = as.integer(units),
         recv_amount = recv_B, recv_ccy = to_ccy, pair = leg$pair)
  } else {
    units_base <- amount_in_A / leg$price
    units <- max(cfg$min_units, floor(units_base))
    if (units <= 0) return(NULL)
    recv_B <- units
    list(instrument = leg$instrument, side = "buy", units = as.integer(units),
         recv_amount = recv_B, recv_ccy = to_ccy, pair = leg$pair)
  }
}

# Sjekk at instrument faktisk finnes/listes av megler
instrument_available <- function(instr, state = .state) {
  syms <- try(state$DS$list_symbols(), silent = TRUE)
  if (inherits(syms, "try-error")) return(TRUE)  # hvis DS ikke støtter listing, ikke blokker
  instr %in% syms
}

# ===== order plumbing =========================================================
.trd_inflight <- FALSE
.trd_last_ts  <- as.numeric(Sys.time()) - 999

.place_leg <- function(instrument, side, units, state = .state) {
  units <- as.integer(units)
  if (!is.finite(units) || units <= 0) {
    return(list(ok = FALSE, err = "invalid_units"))
  }
  err <- NULL; ok <- TRUE
  tryCatch(
    { state$DS$place_market(instrument = instrument, side = side, units = units) },
    error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) }
  )
  list(ok = isTRUE(ok), err = err)
}

.opposite_side <- function(side) if (identical(side, "buy")) "sell" else "buy"

# ===== main execution for one triangle =======================================
execute_triangle <- function(route_vec, notional_S,
                             state = .state,
                             dry_run = NULL) {
  cfg    <- TRD_CFG()
  logger <- getOption("arb.core_logger", function(...) invisible(NULL))
  if (is.null(dry_run)) dry_run <- cfg$dry_run
  
  stopifnot(length(route_vec) == 4, route_vec[1] == route_vec[4])
  S <- route_vec[1]; X <- route_vec[2]; Y <- route_vec[3]
  
  now <- as.numeric(Sys.time())
  if (.trd_inflight || (now - .trd_last_ts) < cfg$cooldown_sec) {
    logger("trade_skip", list(reason = "inflight/cooldown", route = route_vec,
                              cooldown_sec = cfg$cooldown_sec))
    return(FALSE)
  }
  assign(".trd_inflight", TRUE, envir = .GlobalEnv)
  on.exit(assign(".trd_inflight", FALSE, envir = .GlobalEnv), add = TRUE)
  
  # spread gates
  sp1 <- pair_spread_bps2(S, X, state); if (!is.finite(sp1)) { logger("trade_skip", list(reason="spread", leg="S->X", route=route_vec)); return(FALSE) }
  sp2 <- pair_spread_bps2(X, Y, state); if (!is.finite(sp2)) { logger("trade_skip", list(reason="spread", leg="X->Y", route=route_vec)); return(FALSE) }
  sp3 <- pair_spread_bps2(Y, S, state); if (!is.finite(sp3)) { logger("trade_skip", list(reason="spread", leg="Y->S", route=route_vec)); return(FALSE) }
  
  # liquidity + sizing to orders
  if (!has_liquidity(S, X, notional_S, state)) { logger("trade_skip", list(reason="liquidity", leg="S->X", route=route_vec)); return(FALSE) }
  s1 <- amount_to_units(notional_S, S, X, state); if (is.null(s1) || s1$units < cfg$min_units) { logger("trade_skip", list(reason="leg1_units", route=route_vec)); return(FALSE) }
  
  if (!has_liquidity(X, Y, s1$recv_amount, state)) { logger("trade_skip", list(reason="liquidity", leg="X->Y", route=route_vec)); return(FALSE) }
  s2 <- amount_to_units(s1$recv_amount, X, Y, state); if (is.null(s2) || s2$units < cfg$min_units) { logger("trade_skip", list(reason="leg2_units", route=route_vec)); return(FALSE) }
  
  if (!has_liquidity(Y, S, s2$recv_amount, state)) { logger("trade_skip", list(reason="liquidity", leg="Y->S", route=route_vec)); return(FALSE) }
  s3 <- amount_to_units(s2$recv_amount, Y, S, state); if (is.null(s3) || s3$units < cfg$min_units) { logger("trade_skip", list(reason="leg3_units", route=route_vec)); return(FALSE) }
  
  # Sjekk at instrumentene faktisk er listet/handlbare
  if (!instrument_available(s1$instrument, state)) { logger("trade_skip", list(reason="instrument_unavailable", leg="S->X", instrument=s1$instrument, route=route_vec)); return(FALSE) }
  if (!instrument_available(s2$instrument, state)) { logger("trade_skip", list(reason="instrument_unavailable", leg="X->Y", instrument=s2$instrument, route=route_vec)); return(FALSE) }
  if (!instrument_available(s3$instrument, state)) { logger("trade_skip", list(reason="instrument_unavailable", leg="Y->S", instrument=s3$instrument, route=route_vec)); return(FALSE) }
  
  # Logg plan før vi sender
  logger("trade_plan", list(
    route = route_vec,
    legs  = list(
      list(leg="1", side = s1$side, units = s1$units, instrument = s1$instrument),
      list(leg="2", side = s2$side, units = s2$units, instrument = s2$instrument),
      list(leg="3", side = s3$side, units = s3$units, instrument = s3$instrument)
    )
  ))
  
  if (isTRUE(dry_run)) {
    logger("trade_dryrun", list(
      route = route_vec,
      legs  = list(
        list(side = s1$side, units = s1$units, instrument = s1$instrument),
        list(side = s2$side, units = s2$units, instrument = s2$instrument),
        list(side = s3$side, units = s3$units, instrument = s3$instrument)
      )
    ))
    return(TRUE)
  }
  
  # Send legg for legg – logg broker-feil ved miss
  step1 <- .place_leg(s1$instrument, s1$side, s1$units, state)
  if (!step1$ok) { logger("trade_fail", list(stage = "leg1_send", route = route_vec, error = step1$err)); return(FALSE) }
  
  step2 <- .place_leg(s2$instrument, s2$side, s2$units, state)
  if (!step2$ok) {
    .place_leg(s1$instrument, .opposite_side(s1$side), s1$units, state)
    logger("trade_fail", list(stage = "leg2_send_rollback_leg1", route = route_vec, error = step2$err))
    return(FALSE)
  }
  
  step3 <- .place_leg(s3$instrument, s3$side, s3$units, state)
  if (!step3$ok) {
    .place_leg(s2$instrument, .opposite_side(s2$side), s2$units, state)
    .place_leg(s1$instrument, .opposite_side(s1$side), s1$units, state)
    logger("trade_fail", list(stage = "leg3_send_rollback_1_2", route = route_vec, error = step3$err))
    return(FALSE)
  }
  
  assign(".trd_last_ts", now, envir = .GlobalEnv)
  logger("trade", list(result = "filled", route = route_vec,
                       legs = list(
                         list(side = s1$side, units = s1$units, instrument = s1$instrument),
                         list(side = s2$side, units = s2$units, instrument = s2$instrument),
                         list(side = s3$side, units = s3$units, instrument = s3$instrument)
                       )))
  TRUE
}

# ===== Kelly sizing ===========================================================
kelly_size <- function(edge_net_bps, vol_bps_route, bankroll, frac = NULL, cap = NULL) {
  cfg <- TRD_CFG()
  if (is.null(frac)) frac <- cfg$frac_kelly
  if (is.null(cap))  cap  <- cfg$max_notional
  
  # NYTT: bruk fallback-vol ved ikke-positiv vol
  if (!is.finite(vol_bps_route) || vol_bps_route <= 0) {
    vol_bps_route <- getOption("arb.kelly_vol_fallback_bps", getOption("arb.rvbps_fallback_bps", 10.0))
  }
  
  if (!is.finite(edge_net_bps)) return(max(cfg$min_units, 0))  # trygt minimum
  
  bps_scale <- getOption("arb.bps_scale", 1e4)
  var_floor <- getOption("arb.kelly_var_floor", 1e-12)
  mu  <- edge_net_bps / bps_scale
  var <- max((vol_bps_route / bps_scale)^2, var_floor)
  f   <- max(0, min(1, mu / var)) * frac
  size <- bankroll * f
  max(cfg$min_units, min(size, cap))
}

# ===== MAIN (engine-first) ====================================================
maybe_trade_best_triangle <- function(state = .state,
                                      start_currency,
                                      bankroll) {
  logger <- getOption("arb.core_logger", function(...) invisible(NULL))
  cfg    <- TRD_CFG()
  
  stopifnot(is.matrix(state$M), nrow(state$M) > 0)
  if (missing(start_currency)) start_currency <- rownames(state$M)[1]
  if (missing(bankroll))        bankroll <- getOption("arb.bankroll_default", 10000)
  
  # 0) warm vol buffer
  if (length(state$latest)) for (p in names(state$latest)) push_mid(p, q_mid(p, state))
  
  # 1) ensure M/BID/ASK
  M <- ensure_M(state)
  .ensure_idx_cache(state); .init_cap_matrix(state)
  
  # 2) engine reinit/refresh/mark-changes
  engine_reinit_if_needed(state, start_currency)
  engine_refresh_mats(state)
  engine_mark_m_changes(prevM = state$M_prev, newM = state$M)
  state$M_prev <- state$M
  
  # 3) evaluer top-K fra engine
  res <- engine_eval_topK(
    K = getOption("arb.topK.print", 25L),
    fee_pct = getOption("arb.fee_pct", 0),
    slip_base_frac = (getOption("arb.assumed_slip_bps", 0))/1e4,
    buf_base_frac  = (getOption("arb.safety_buffer_bps", 0))/1e4,
    k_slip = getOption("arb.k_slip", 0.30),
    rvbps_fallback_bps = getOption("arb.rvbps_fallback_bps", 10.0),
    start_qty_base = bankroll
  )
  
  if (is.null(res) || !length(res$idx)) {
    logger("trade_skip", list(
      reason    = "no_candidates_from_engine",
      start_ccy = start_currency,
      bankroll  = bankroll
    ))
    return(invisible(FALSE))
  }
  
  traded <- FALSE
  now_ts <- as.numeric(Sys.time())
  since_last_base <- now_ts - .trd_last_ts
  
  # 4) loop kandidater
  N <- length(res$idx)
  for (i in seq_len(N)) {
    mid <- state$ccys[ res$mid_i[i] ]
    end <- state$ccys[ res$end_i[i] ]
    route_vec <- c(start_currency, mid, end, start_currency)
    
    # dyn edge / threshold
    fee_pct_opt           <- getOption("arb.fee_pct", 0)
    assumed_slip_bps_opt  <- getOption("arb.assumed_slip_bps", 0)
    safety_buffer_bps_opt <- getOption("arb.safety_buffer_bps", 0)
    slip_base_frac <- (assumed_slip_bps_opt %||% 0)/1e4
    buf_base_frac  <- (safety_buffer_bps_opt %||% 0)/1e4
    
    pairs <- c(paste0(route_vec[1],"/",route_vec[2]),
               paste0(route_vec[2],"/",route_vec[3]),
               paste0(route_vec[3],"/",route_vec[1]))
    rvbps <- route_vol_bps(pairs)
    k_slip <- getOption("arb.k_slip", 0.30)
    slip_dyn_frac <- slip_base_frac + if (is.finite(rvbps)) k_slip * (rvbps/1e4) else 0
    thr_dyn_frac  <- edge_trigger_threshold(fee_pct_opt, slip_dyn_frac, buf_base_frac)
    
    # edge i motor er (prod/dir - 1); bruk edge - threshold
    dyn_edge_bps <- 1e4 * ((res$edge[i] %||% NA_real_) - thr_dyn_frac)
    if (!is.finite(dyn_edge_bps) || dyn_edge_bps < cfg$min_edge_bps) next
    
    # quick gates
    if (!is.finite(pair_spread_bps2(start_currency, mid, state))) next
    if (!is.finite(pair_spread_bps2(mid,           end, state)))  next
    if (!is.finite(pair_spread_bps2(end, start_currency, state))) next
    
    if (!has_liquidity(start_currency, mid,  state = state)) next
    if (!has_liquidity(mid,           end,  state = state)) next
    if (!has_liquidity(end,           start_currency, state = state)) next
    
    # sizing (Kelly)
    rvbps_eff <- if (is.finite(rvbps)) rvbps else getOption("arb.rvbps_fallback_bps", 10.0)
    size_S <- kelly_size(edge_net_bps = dyn_edge_bps, vol_bps_route = rvbps_eff, bankroll = bankroll)
    
    # sanity-PnL
    ok_trade <- FALSE
    t0 <- proc.time()[3]; res_pnl <- NULL
    tryCatch({ res_pnl <- tri_arbitrage_pnl(start_currency, mid, end, state = state) },
             error = function(e) res_pnl <<- list(ok = FALSE, err = conditionMessage(e)))
    dt <- proc.time()[3] - t0
    
    if (is.list(res_pnl) && isTRUE(res_pnl$ok) && is.finite(res_pnl$pnl_after_costs) && res_pnl$pnl_after_costs > 0) {
      t1 <- proc.time()[3]; err2 <- NULL
      tryCatch({ ok_trade <- execute_triangle(route_vec, size_S, state = state) },
               error = function(e) { err2 <<- conditionMessage(e); ok_trade <<- FALSE })
      dt2 <- proc.time()[3] - t1
      if (ok_trade) {
        logger("trade", list(
          result       = "attempt_ok",
          route        = route_vec,
          dyn_edge_bps = dyn_edge_bps,
          vol_bps      = rvbps_eff,
          size_start   = size_S,
          start_ccy    = route_vec[1],
          time_eval_s  = dt,
          time_exec_s  = dt2,
          dry_run      = TRD_CFG()$dry_run
        ))
        traded <- TRUE
        break
      } else {
        logger("trade_fail", list(
          result       = "order_send_fail",
          error        = err2 %||% "unknown",
          route        = route_vec,
          dyn_edge_bps = dyn_edge_bps,
          size_start   = size_S,
          start_ccy    = route_vec[1],
          time_eval_s  = dt,
          dry_run      = TRD_CFG()$dry_run
        ))
      }
    } else {
      logger("trade_skip", list(
        reason        = "sanity/cooldown/liquidity/order",
        route         = route_vec,
        dyn_edge_bps  = dyn_edge_bps,
        vol_bps       = rvbps_eff,
        size_start    = size_S,
        inflight      = .trd_inflight,
        since_last_s  = since_last_base,
        time_eval_s   = dt,
        dry_run       = TRD_CFG()$dry_run
      ))
    }
  }
  
  invisible(traded)
}