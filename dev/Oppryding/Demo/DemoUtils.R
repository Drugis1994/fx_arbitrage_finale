# ===============================
#  Runtime state & core utilities
# ===============================

# --- utils.R :: PRECOMPUTE HELPERS (kept minimal) ---
precompute_route_idx <- function(ccys, start_ccy) {
  others <- setdiff(ccys, start_ccy)
  if (length(others) < 2L) {
    out <- matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("mid_i","end_i")))
    storage.mode(out) <- "integer"
    return(out)
  }
  idx <- match(others, ccys)
  
  m <- length(idx)
  # KORRIGERT: bruk rep() for `each=`, ikke rep.int()
  mid  <- rep(idx, each = m)
  end  <- rep(idx, times = m)
  keep <- mid != end
  
  out <- cbind(mid[keep], end[keep])
  colnames(out) <- c("mid_i","end_i")
  storage.mode(out) <- "integer"
  out
}

# Debug helper (opt-in): set options(tri.debug = TRUE) to enable noisy logs
tri_dbg <- function(fmt, ...) {
  if (isTRUE(getOption("tri.debug", FALSE))) {
    cat(sprintf(paste0("[DBG] ", fmt, "\n"), ...))
  }
}

# new_runtime_state()
# -------------------
# A tiny container for all data that changes while the program runs.
# We keep environments so we can mutate by reference and avoid copies.
new_runtime_state <- function() {
  tri_dbg("new_runtime_state(): creating fresh runtime state")
  list(
    latest     = new.env(parent = emptyenv()),   # quotes keyed by pair "BASE/QUOTE"
    episodes   = new.env(parent = emptyenv()),   # active per-route episodes
    last_line  = new.env(parent = emptyenv()),   # print-dedup cache
    qm         = NULL,                           # cached quote map (env)
    M          = NULL,                           # cached mids matrix
    BID        = NULL,
    ASK        = NULL,
    ccys       = NULL,
    route_idx  = NULL,
    DS         = NULL,                              
    ob_cache   = new.env(parent = emptyenv())        
  )
}

# Default singleton (used when a function doesn't receive an explicit state)
.state <- new_runtime_state()


# =====================
#  Quote math utilities
# =====================

# invert_quote()
invert_quote <- function(q){
  if (!is.list(q) || !is.finite(q$bid) || !is.finite(q$ask) || q$bid <= 0 || q$ask <= 0)
    return(NULL)
  list(bid = 1/q$ask, ask = 1/q$bid, mid = 0.5 * (1/q$ask + 1/q$bid))
}

# cross_via_usd()
cross_via_usd <- function(q_base_usd, q_quote_usd){
  list(
    bid = q_base_usd$bid / q_quote_usd$ask,
    ask = q_base_usd$ask / q_quote_usd$bid,
    mid = 0.5 * ((q_base_usd$bid / q_quote_usd$ask) + (q_base_usd$ask / q_quote_usd$bid))
  )
}

# build_quote_map()
build_quote_map <- function(latest_by_pair){
  tri_dbg("build_quote_map(): seeding from %d direct pairs", length(latest_by_pair))
  qm <- new.env(parent = emptyenv())
  for (pair in names(latest_by_pair)){
    px  <- latest_by_pair[[pair]]
    bq  <- strsplit(pair, "/", fixed = TRUE)[[1]]
    key <- paste(bq[1], bq[2], sep = "/")
    qm[[key]] <- list(bid = px$bid, ask = px$ask, mid = 0.5 * (px$bid + px$ask))
  }
  cnt_inv <- 0L
  for (key in ls(qm)) {
    sp <- strsplit(key, "/", fixed = TRUE)[[1]]
    inv_key <- paste(sp[2], sp[1], sep = "/")
    if (is.null(qm[[inv_key]])) {
      qinv <- invert_quote(qm[[key]])
      if (!is.null(qinv)) { qm[[inv_key]] <- qinv; cnt_inv <- cnt_inv + 1L }
    }
  }
  tri_dbg("build_quote_map(): added %d inferred inverses", cnt_inv)
  ccys <- sort(unique(unlist(strsplit(ls(qm), "/"))))
  if ("USD" %in% ccys) {
    cnt_cross <- 0L
    for (a in ccys) for (b in ccys) {
      if (a == b) next
      key <- paste(a, b, sep = "/")
      if (!is.null(qm[[key]])) next
      k1 <- paste(a, "USD", sep = "/"); k2 <- paste(b, "USD", sep = "/")
      if (!is.null(qm[[k1]]) && !is.null(qm[[k2]])) { qm[[key]] <- cross_via_usd(qm[[k1]], qm[[k2]]); cnt_cross <- cnt_cross + 1L }
    }
    tri_dbg("build_quote_map(): synthesized %d crosses via USD", cnt_cross)
  } else {
    tri_dbg("build_quote_map(): USD not present → no synthetic crosses")
  }
  qm
}

# make_price_matrices()
make_price_matrices <- function(qm) {
  keys  <- ls(qm)
  parts <- strsplit(keys, "/", fixed = TRUE)
  base  <- vapply(parts, `[`, character(1), 1)
  quote <- vapply(parts, `[`, character(1), 2)
  ccys <- sort(unique(c(base, quote)))
  n <- length(ccys)
  M   <- matrix(NA_real_, n, n, dimnames = list(ccys, ccys))
  BID <- matrix(NA_real_, n, n, dimnames = list(ccys, ccys))
  ASK <- matrix(NA_real_, n, n, dimnames = list(ccys, ccys))
  # Sett bare M-diagonalen til 1. La BID/ASK-diagonalen være NA for å unngå utilsiktet bruk.
  diag(M) <- 1
  diag(BID) <- NA_real_
  diag(ASK) <- NA_real_
  ib <- match(base,  ccys)
  iq <- match(quote, ccys)
  bids <- vapply(keys, function(k) qm[[k]]$bid, numeric(1))
  asks <- vapply(keys, function(k) qm[[k]]$ask, numeric(1))
  mid_ok <- is.finite(bids) & is.finite(asks)
  if (any(mid_ok)) M[cbind(ib[mid_ok], iq[mid_ok])] <- (bids[mid_ok] + asks[mid_ok]) * 0.5
  bid_ok <- is.finite(bids)
  if (any(bid_ok)) BID[cbind(ib[bid_ok], iq[bid_ok])] <- bids[bid_ok]
  ask_ok <- is.finite(asks)
  if (any(ask_ok)) ASK[cbind(ib[ask_ok], iq[ask_ok])] <- asks[ask_ok]
  tri_dbg("make_price_matrices(): built %dx%d matrices", n, n)
  list(M = M, BID = BID, ASK = ASK)
}

# Effective three-leg fee and edge trigger helper
eff_fee_3legs <- function(f){ 1 - (1 - f)^3 }

edge_trigger_threshold <- function(fee_pct, slip_frac, buffer_frac){
  # Inputs are fractions (not bps)
  eff_fee <- eff_fee_3legs(fee_pct)
  thr <- eff_fee + 3 * slip_frac + buffer_frac
  tri_dbg("edge_trigger_threshold(): eff_fee=%.6f, slip=%.6f, buffer=%.6f → thr=%.6f",
          eff_fee, slip_frac, buffer_frac, thr)
  thr
}


# ==============================
#  Logging (atomic CSV appends)
# ==============================

safe_write <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock_path <- paste0(path, ".lock")
  have_lock <- FALSE
  for (i in 1:200) {
    if (!file.exists(lock_path)) { writeLines(sprintf("pid=%s time=%s", Sys.getpid(), Sys.time()), lock_path); have_lock <- TRUE; break }
    Sys.sleep(0.01)
  }
  on.exit({ if (have_lock && file.exists(lock_path)) unlink(lock_path, force = TRUE) }, add = TRUE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".csv")
  if (!file.exists(path)) {
    write.table(df, file = tmp, sep = ",", row.names = FALSE, col.names = TRUE,  append = FALSE, na = "")
    file.rename(tmp, path)
  } else {
    write.table(df, file = tmp, sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE, na = "")
    file.append(path, tmp)
    unlink(tmp)
  }
  tri_dbg("safe_write(): wrote %d rows to %s", nrow(df), basename(path))
}

new_episode_id <- function(rute) {
  ts <- format(lubridate::now(tzone = "UTC"), "%Y%m%d%H%M%OS3")
  paste0(gsub("[^A-Z0-9]+", "", toupper(rute)), "_", ts)
}

log_signal <- function(episode_id, ts, mode, rute, start_ccy, volum_base,
                       fee_pct, assumed_slip_bps, safety_buffer_bps,
                       direct_mid, via_mid, me_mid,
                       edge_no_fee_bps, trigger_bps,
                       s1_used, s2_used, s3_used,
                       qty_mid, qty_end, final_base_est,
                       file = file.path(ROOT, "data", "signals_live_ready.csv")) {
  row <- data.frame(
    episode_id = episode_id, ts = format(ts, "%Y-%m-%d %H:%M:%OS3"),
    mode = mode, rute = rute, start_ccy = start_ccy, volum_base = volum_base,
    fee_pct = fee_pct, assumed_slip_bps = assumed_slip_bps, safety_buffer_bps = safety_buffer_bps,
    direct_mid = direct_mid, via_mid = via_mid, me_mid = me_mid,
    edge_no_fee_bps = edge_no_fee_bps, trigger_bps = trigger_bps,
    leg1 = s1_used, leg2 = s2_used, leg3 = s3_used,
    qty_mid = qty_mid, qty_end = qty_end, final_base_est = final_base_est,
    stringsAsFactors = FALSE
  )
  safe_write(row, file)
}

profit_log_file <- file.path(ROOT, "data", "profit_events.csv")
dir.create(dirname(profit_log_file), showWarnings = FALSE, recursive = TRUE)

logg_profit_event <- function(event, episode_id, rute,
                              netPnL = NA_real_, pnl_pct = NA_real_,
                              final_base = NA_real_,
                              edge_bps = NA_real_, fee_pct = NA_real_,
                              direct_mid = NA_real_, via_mid = NA_real_, me_mid = NA_real_,
                              qty_mid = NA_real_, qty_end = NA_real_,
                              leg1_used = NA_character_, leg2_used = NA_character_, leg3_used = NA_character_,
                              varighet_s = NA_real_) {
  row <- data.frame(
    tidspunkt   = format(lubridate::now(), "%Y-%m-%d %H:%M:%OS3"),
    event       = event,
    episode_id  = episode_id,
    rute        = rute,
    netPnL      = round(netPnL, 2),
    pnl_pct     = round(pnl_pct, 6),
    final_base  = round(final_base, 2),
    edge_bps    = if (is.na(edge_bps)) NA_real_ else round(edge_bps, 3),
    fee_pct     = fee_pct,
    direct_mid  = direct_mid, 
    via_mid     = via_mid, 
    me_mid      = me_mid,
    qty_mid     = qty_mid, 
    qty_end     = qty_end,
    leg1_used   = leg1_used, 
    leg2_used   = leg2_used, 
    leg3_used   = leg3_used,
    varighet_s  = if (is.na(varighet_s)) NA_real_ else round(varighet_s, 3),
    stringsAsFactors = FALSE
  )
  safe_write(row, profit_log_file)
}

log_exec <- function(episode_id, leg, from_ccy, to_ccy, side, px, qty_from, qty_to,
                     order_id = NA, status = NA, fee_paid = NA, slippage_bps = NA,
                     t_quote = NA, t_send = NA, t_ack = NA, t_fill = NA, err = NA,
                     file = file.path(ROOT, "data", "executions_live_ready.csv")) {
  row <- data.frame(
    episode_id = episode_id, leg = leg, from = from_ccy, to = to_ccy, side = side, px = px,
    qty_from = qty_from, qty_to = qty_to, order_id = order_id, status = status,
    fee_paid = fee_paid, slippage_bps = slippage_bps,
    t_quote = t_quote, t_send = t_send, t_ack = t_ack, t_fill = t_fill, err = err,
    stringsAsFactors = FALSE
  )
  safe_write(row, file)
}

logg_arbitrasje <- function(event, rute,
                            direkte_kurs = NA_real_,
                            indirekte_kurs = NA_real_,
                            netPnL = NA_real_, pnl_pct = NA_real_,
                            edge_bps = NA_real_, trigger_bps = NA_real_,
                            final_base = NA_real_,
                            varighet_s = NA_real_) {
  row <- data.frame(
    tidspunkt   = format(lubridate::now(), "%Y-%m-%d %H:%M:%OS3"),
    mode        = MODE,
    event       = event,            # START_PROFIT / START_EDGE / END
    rute        = rute,
    direkte_kurs   = direkte_kurs,
    indirekte_kurs = indirekte_kurs,
    netPnL      = round(netPnL, 2),
    pnl_pct     = round(pnl_pct, 6),
    edge_bps    = round(edge_bps, 3),
    trigger_bps = round(trigger_bps, 3),
    final_base  = round(final_base, 2),
    varighet_s  = round(varighet_s, 3),
    stringsAsFactors = FALSE
  )
  safe_write(row, loggfil)
}


# ==============================
#  Small UX helpers
# ==============================

emit_once <- function(txt, state = .state) {
  if (is.null(state$last_line$txt)) state$last_line$txt <- ""
  if (!identical(state$last_line$txt, txt)) { cat(txt); state$last_line$txt <- txt }
}

# Optional helper (not required by evaluate_all, but handy elsewhere)
side_used <- function(side, from_i, to_i, ccys, BID, ASK) {
  if (is.na(side)) return(list(used = "none", px = NA_real_))
  if (side == 0L) list(used = sprintf("ask %s/%s", ccys[to_i], ccys[from_i]), px = ASK[to_i, from_i])
  else            list(used = sprintf("bid %s/%s", ccys[from_i], ccys[to_i]), px = BID[from_i, to_i])
}


# ==============================
#  EVALUER ALLE OG PRINT
# ==============================

evaluate_all <- function(state = .state) {
  # 1) Rehydrate + validate quotes
  nm <- ls(envir = state$latest)
  if (!length(nm)) return(invisible(NULL))
  latest_list <- lapply(nm, function(k) state$latest[[k]])
  names(latest_list) <- nm
  ok <- vapply(latest_list, function(x) is.list(x) && !is.null(x$bid) && !is.null(x$ask) &&
                 is.finite(x$bid) && is.finite(x$ask), logical(1))
  if (!any(ok)) return(invisible(NULL))
  latest_list <- latest_list[ok]
  
  # 2) Build quote-map + pris-matriser (mid/bid/ask) i ett
  qm <- build_quote_map(latest_list)
  if (!length(ls(qm))) return(invisible(NULL))
  pm  <- make_price_matrices(qm)
  M   <- pm$M; BID <- pm$BID; ASK <- pm$ASK
  ccys <- rownames(M)
  if (is.null(ccys) || !(start_ccy %in% ccys)) return(invisible(NULL))
  state$qm <- qm; state$M <- M; state$BID <- BID; state$ASK <- ASK; state$ccys <- ccys
  
  # 3) Precompute/ensure route_idx (1-basert matrise [mid_i, end_i])
  if (is.null(state$route_idx) || !is.matrix(state$route_idx) || ncol(state$route_idx) < 2L ||
      !identical(rownames(M), state$ccys)) {
    state$route_idx <- precompute_route_idx(ccys, start_ccy)
  }
  route_idx <- state$route_idx
  if (!nrow(route_idx)) return(invisible(NULL))
  
  # 4) C++: mids-only edge screen på alle ruter
  s_i <- match(start_ccy, ccys)  # 1-basert
  es  <- edge_screen_cpp(s_i, route_idx, M)  # mid_i/end_i/dmid/vmid/mmid/edge
  es <- as.data.frame(es, stringsAsFactors = FALSE)
  ok_edge <- is.finite(es$edge)
  if (!any(ok_edge)) return(invisible(NULL))
  es <- es[ok_edge, , drop = FALSE]
  ord <- order(-es$edge, es$mid_i, es$end_i)
  es  <- es[ord, , drop = FALSE]
  
  # 5) Begrens evalueringsmengde / utskrift
  K_eval  <- getOption("arb.topK.eval",  500L)
  K_print <- getOption("arb.topK.print",  25L)
  es_eval <- utils::head(es, K_eval)
  
  found_pnl  <- FALSE
  found_edge <- FALSE
  
  # ---- SLIP/THRESHOLD (fra options; enheter i bps → deles på 1e4) ----
  k_slip <- 0.30
  slip_base_bps <- getOption("arb.assumed_slip_bps", 0)
  buf_base_bps  <- getOption("arb.safety_buffer_bps", 2)
  # -------------------------------------------------------------------
  
  n_routes <- 0L; sum_ms <- 0.0; max_ms <- 0.0; n_printed <- 0L
  
  # PRECOMPUTE small string pieces used in the loop
  sc_slash  <- paste0(start_ccy, "/")  # "USD/"
  slash_sc  <- paste0("/", start_ccy)  # "/USD"
  
  for (k in seq_len(nrow(es_eval))) {
    mid_i <- es_eval$mid_i[k]; end_i <- es_eval$end_i[k]
    mid   <- ccys[mid_i];       end   <- ccys[end_i]
    direct_mid <- es_eval$dmid[k]; via_mid <- es_eval$vmid[k]; me_mid <- es_eval$mmid[k]
    edge_no_fee <- es_eval$edge[k]
    
    # --- route-vol → slip i fraksjon, og threshold i fraksjon ---
    pairs <- c(paste0(sc_slash, mid),      # start_ccy/mid
               paste0(mid, "/", end),      # mid/end
               paste0(end, slash_sc))      # end/start_ccy
    rvbps <- route_vol_bps(pairs)                 # i bps
    slip_base_frac <- slip_base_bps / 1e4        # bps → fraksjon
    buf_base_frac  <- buf_base_bps  / 1e4        # bps → fraksjon
    slip_dyn       <- slip_base_frac + if (is.finite(rvbps)) k_slip * (rvbps/1e4) else 0
    threshold_dyn  <- edge_trigger_threshold(fee_pct, slip_dyn, buf_base_frac)
    
    t0 <- Sys.time()
    pnl_df <- tri_pnl_cpp(
      start_i_1b     = s_i,
      route_idx      = matrix(as.integer(c(mid_i, end_i)), ncol = 2),
      BID            = BID,
      ASK            = ASK,
      start_qty_base = volum_base,
      fee_pct        = fee_pct
    )
    t_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    
    n_routes <- n_routes + 1L; sum_ms <- sum_ms + t_ms; if (t_ms > max_ms) max_ms <- t_ms
    if (!nrow(pnl_df) || !isTRUE(pnl_df$ok[1])) next
    
    final_base <- pnl_df$final_base[1]
    netPnL     <- pnl_df$netPnL[1]
    pnl_pct    <- pnl_df$pnl_pct[1]
    
    u1 <- side_used(pnl_df$leg1_side[1], s_i,   mid_i, ccys, BID, ASK)
    u2 <- side_used(pnl_df$leg2_side[1], mid_i, end_i, ccys, BID, ASK)
    u3 <- side_used(pnl_df$leg3_side[1], end_i, s_i,   ccys, BID, ASK)
    
    used1 <- u1$used; used2 <- u2$used; used3 <- u3$used
    is_prof <- is.finite(netPnL) && (netPnL > 0)
    is_edge <- is.finite(edge_no_fee) && (edge_no_fee > threshold_dyn)
    if (is_prof) found_pnl  <- TRUE
    if (is_edge) found_edge <- TRUE
    
    tag  <- if (is_prof) "✅" else if (is_edge) "⚠️" else "ℹ️"
    rute <- paste(start_ccy, "->", mid, "->", end)
    
    if (is_prof || is_edge) {
      if (is.null(state$episodes[[rute]])) {
        eps_id <- new_episode_id(rute)
        state$episodes[[rute]] <- list(start = lubridate::now(), episode_id = eps_id)
        state$episodes[[rute]]$profit_episode <- is_prof
        if (is_prof) {
          logg_profit_event(
            event       = "START_PROFIT",
            episode_id  = eps_id,
            rute        = rute,
            netPnL      = netPnL,
            pnl_pct     = pnl_pct,
            final_base  = final_base,
            edge_bps    = if (is.finite(edge_no_fee)) 10000 * edge_no_fee else NA_real_,
            fee_pct     = fee_pct,
            direct_mid  = direct_mid,
            via_mid     = via_mid,
            me_mid      = me_mid,
            qty_mid     = NA_real_,
            qty_end     = NA_real_,
            leg1_used   = used1,
            leg2_used   = used2,
            leg3_used   = used3
          )
        }
        log_signal(
          episode_id = eps_id, ts = lubridate::now(), mode = MODE, rute = rute,
          start_ccy = start_ccy, volum_base = volum_base,
          fee_pct = fee_pct,
          assumed_slip_bps   = slip_base_bps,   # <- var slip_base
          safety_buffer_bps  = buf_base_bps,    # <- var buf_base
          direct_mid = direct_mid, via_mid = via_mid, me_mid = me_mid,
          edge_no_fee_bps = if (is.finite(edge_no_fee)) 10000 * edge_no_fee else NA_real_,
          trigger_bps = 10000 * threshold_dyn,
          s1_used = used1, s2_used = used2, s3_used = used3,
          qty_mid = NA_real_, qty_end = NA_real_,
          final_base_est = final_base
        )
        logg_arbitrasje(
          event = if (is_prof) "START_PROFIT" else "START_EDGE",
          rute  = rute,
          direkte_kurs   = direct_mid,
          indirekte_kurs = via_mid * me_mid,
          netPnL   = netPnL,
          pnl_pct  = pnl_pct,
          edge_bps = if (is.finite(edge_no_fee)) 10000 * edge_no_fee else NA_real_,
          trigger_bps = 10000 * threshold_dyn,
          final_base  = final_base
        )
      }
    } else {
      if (!is.null(state$episodes[[rute]])) {
        varighet <- as.numeric(difftime(lubridate::now(), state$episodes[[rute]]$start, units = "secs"))
        if (isTRUE(state$episodes[[rute]]$profit_episode)) {
          ep_id <- state$episodes[[rute]]$episode_id
          logg_profit_event(
            event       = "END",
            episode_id  = ep_id,
            rute        = rute,
            netPnL      = netPnL,
            pnl_pct     = pnl_pct,
            final_base  = final_base,
            edge_bps    = if (is.finite(edge_no_fee)) 10000 * edge_no_fee else NA_real_,
            fee_pct     = fee_pct,
            direct_mid  = direct_mid,
            via_mid     = via_mid,
            me_mid      = me_mid,
            qty_mid     = NA_real_,
            qty_end     = NA_real_,
            leg1_used   = used1,
            leg2_used   = used2,
            leg3_used   = used3,
            varighet_s  = varighet
          )
        }
        logg_arbitrasje(
          event = "END",
          rute  = rute,
          direkte_kurs   = direct_mid,
          indirekte_kurs = via_mid * me_mid,
          netPnL   = netPnL,
          pnl_pct  = pnl_pct,
          edge_bps = if (is.finite(edge_no_fee)) 10000 * edge_no_fee else NA_real_,
          trigger_bps = 10000 * threshold_dyn,
          final_base  = final_base,
          varighet_s  = varighet
        )
        state$episodes[[rute]] <- NULL
      }
    }
    
    # 8) Skriv statuslinje (med tid per rute)
    if (n_printed < K_print) {
      edge_pct   <- ifelse(is.finite(edge_no_fee), 100 * edge_no_fee, NA_real_)
      thresh_pct <- 100 * threshold_dyn
      line <- sprintf(
        "%s %s | Final %s: %.2f | Net PnL: %.2f (%.3f%%) | Edge(before bid/ask cost): %s | Trigger: %.3f%% | ⏱ %.2f ms\n",
        tag, rute, start_ccy,
        final_base,
        netPnL, pnl_pct,
        if (is.na(edge_pct)) "NA" else sprintf("%.3f%%", edge_pct),
        thresh_pct,
        t_ms
      )
      if (exists("only_print_when_edge_or_profit") && isTRUE(only_print_when_edge_or_profit)) {
        if (is_prof || is_edge) emit_once(line, state)
      } else {
        cat(line)
      }
      n_printed <- n_printed + 1L
    }
  }
  
  # 9) Post-tick summary
  if (n_routes > 0L) {
    avg_ms <- sum_ms / n_routes
    cat(sprintf("⏱ Routes: %d | Avg: %.2f ms | Max: %.2f ms\n", n_routes, avg_ms, max_ms))
  }
  if (!found_pnl) {
    cat("❌ Ingen lønnsom arbitrasje (Net PnL > 0) i denne oppdateringen.\n")
    topN <- getOption("arb.topN", if (exists("TOP_N_CANDIDATES")) TOP_N_CANDIDATES else 10)
    if (exists("show_top_candidates") && is.function(show_top_candidates)) {
      try(show_top_candidates(topN, metric = "pnl", state = state), silent = TRUE)
    }
  }
  invisible(NULL)
}