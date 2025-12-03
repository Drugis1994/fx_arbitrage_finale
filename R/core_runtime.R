# --- Convert "EUR_USD" → "EUR/USD"
.cr_sym_to_pair <- function(sym) gsub("_", "/", sym, fixed = TRUE)

# --- Convert "EUR/USD" → "EUR_USD"
.cr_pair_to_sym <- function(pair) gsub("/", "_", pair, fixed = TRUE)

# --- Extract sorted list of currencies
infer_ccys_from_instruments <- function(instruments) {
  pairs <- vapply(instruments, .cr_sym_to_pair, character(1))
  parts <- strsplit(pairs, "/", fixed = TRUE)
  ccys  <- unique(unlist(parts, use.names = FALSE))
  sort(ccys[nzchar(ccys)])
}

# ---- liquidity gate (TTL cached) ----
has_liquidity <- function(from_ccy, to_ccy, state, ttl_ms = 300L) {
  key   <- paste0(from_ccy, "/", to_ccy)
  instr <- paste0(from_ccy, "_", to_ccy)
  
  now <- as.numeric(Sys.time()) * 1000
  oc <- state$ob_cache[[key]]
  if (!is.null(oc) && (now - oc$ts) < ttl_ms) return(isTRUE(oc$ok))
  
  ob <- .ds_orderbook_safe(state$DS, instr)
  ok <- is.list(ob) && length(ob) > 0
  
  state$ob_cache[[key]] <- list(ts = now, ok = ok)
  ok
}

# ---- tick handler (direct + inverse, fully consistent) ----
make_on_tick <- function(state) {
  function(sym, bid, ask, time) {
    
    if (identical(sym, "HEARTBEAT")) return(invisible())
    if (!is.finite(bid) || !is.finite(ask) || bid <= 0 || ask <= 0) return(invisible())
    
    # Normaliser
    pair <- .cr_sym_to_pair(sym)
    
    idx <- state$pair_idx[[pair]]
    if (is.null(idx)) return(invisible())
    
    a <- idx$i; b <- idx$j
    
    # --- direct update ---
    state$latest[[pair]] <- list(bid = bid, ask = ask, time = time)
    engine_push_tick(state$eng, a, b, bid, ask)
    
    state$BID[a, b] <- bid
    state$ASK[a, b] <- ask
    state$M[a, b]   <- sqrt(bid * ask)
    
    # --- inverse update (ONLY if not streamed) ---
    parts <- strsplit(pair, "/", fixed = TRUE)[[1]]
    inv_pair <- paste0(parts[2], "/", parts[1])
    
    if (!isTRUE(state$instr_set_slash[[inv_pair]])) {
      inv_idx <- state$pair_idx[[inv_pair]]
      if (!is.null(inv_idx)) {
        
        bid_inv <- 1 / ask
        ask_inv <- 1 / bid
        
        if (is.finite(bid_inv) && is.finite(ask_inv)) {
          ia <- inv_idx$i; ib <- inv_idx$j
          
          state$latest[[inv_pair]] <- list(
            bid = bid_inv,
            ask = ask_inv,
            time = time
          )
          
          engine_push_tick(state$eng, ia, ib, bid_inv, ask_inv)
          
          state$BID[ia, ib] <- bid_inv
          state$ASK[ia, ib] <- ask_inv
          state$M[ia, ib]   <- sqrt(bid_inv * ask_inv)
        }
      }
    }
    
    invisible()
  }
}


  
 




init_state <- function(DS, instruments, start_ccy = "USD") {
  
  # Valutaunivers
  ccys <- infer_ccys_from_instruments(instruments)
  
  n <- length(ccys)
  BID <- matrix(NA_real_, n, n, dimnames=list(ccys, ccys))
  ASK <- matrix(NA_real_, n, n, dimnames=list(ccys, ccys))
  M   <- matrix(NA_real_, n, n, dimnames=list(ccys, ccys))
  
  # Bygg forward+inverse mapping
  pair_idx <- list()
  for (sym in instruments) {
    p <- .cr_sym_to_pair(sym)  # EUR_USD → EUR/USD
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    a <- parts[1]; b <- parts[2]
    
    pair_idx[[p]] <- list(i = which(ccys == a), j = which(ccys == b))
    inv <- paste0(b, "/", a)
    
    pair_idx[[inv]] <- list(i = which(ccys == b), j = which(ccys == a))
  }
  
  # 🔥 sanne triangler (ikke syntetiske)
 # routes <- build_true_triangles(instruments)
  
  state <- new.env(parent = emptyenv())
  state$DS        <- DS
  state$ccys      <- ccys
  state$BID       <- BID
  state$ASK       <- ASK
  state$M         <- M
  state$pair_idx  <- pair_idx
  state$routes    <- NULL
  state$start_ccy <- start_ccy
  state$latest    <- new.env()
  state$ob_cache  <- new.env()
  
  # Sett med instrumenter (underscore → ekte OANDA-stream)
  state$instr_set <- as.list(setNames(rep(TRUE, length(instruments)), instruments))
  
  # Samme som slash
  instr_slash <- vapply(instruments, .cr_sym_to_pair, character(1))
  state$instr_set_slash <- as.list(setNames(rep(TRUE, length(instr_slash)), instr_slash))
  
  state
}

debug_route <- function(state, route_row) {
  ccys <- state$ccys
  
  S <- route_row$S
  X <- route_row$X
  Y <- route_row$Y
  
  cS <- ccys[S]; cX <- ccys[X]; cY <- ccys[Y]
  
  rd <- function(a, b) {
    bid <- state$BID[a,b]
    ask <- state$ASK[a,b]
    list(bid = bid, ask = ask)
  }
  
  leg1 <- rd(S,X)
  leg2 <- rd(X,Y)
  leg3 <- rd(Y,S)
  
  f1 <- if (is.finite(leg1$ask)) 1/leg1$ask else NA
  f2 <- if (is.finite(leg2$ask)) 1/leg2$ask else NA
  f3 <- if (is.finite(leg3$bid)) leg3$bid   else NA
  
  list(
    legs = list(leg1 = leg1, leg2 = leg2, leg3 = leg3),
    product = f1*f2*f3
  )
}









diagnose_missing_triangles <- function(instruments) {
  pairs <- vapply(instruments, function(x) gsub("_", "/", x), character(1))
  parts <- strsplit(pairs, "/", fixed = TRUE)
  ccys  <- sort(unique(unlist(parts)))
  
  has_pair <- function(a, b) paste0(a, "/", b) %in% pairs
  
  cat("\n🔍 TRIANGLE DIAGNOSIS\n")
  found_any <- FALSE
  
  for (a in ccys) {
    for (b in ccys) {
      if (a == b) next
      if (!has_pair(a, b)) next
      
      for (c in ccys) {
        if (c %in% c(a, b)) next
        
        leg1 <- has_pair(a, b)
        leg2 <- has_pair(b, c)
        leg3 <- has_pair(c, a)
        
        # If at least 1 missing, print diagnosis
        if (!(leg1 && leg2 && leg3)) {
          found_any <- TRUE
          cat(sprintf(
            "\nTriangle candidate: %s → %s → %s → %s\n", a, b, c, a
          ))
          cat(sprintf("   %s/%s : %s\n", a, b,  if (leg1) "OK" else "❌ MISSING"))
          cat(sprintf("   %s/%s : %s\n", b, c,  if (leg2) "OK" else "❌ MISSING"))
          cat(sprintf("   %s/%s : %s\n", c, a,  if (leg3) "OK" else "❌ MISSING"))
        }
      }
    }
  }
  
  if (!found_any)
    cat("✅ Ingen manglende ben. Alle triangler er fullstendige!\n")
  
  invisible(TRUE)
}

