# ~/Documents/FX_Ferdig_project/R/Order.R
# Order.R — funksjoner for orderbook / liquidity / rebalancing
# Safe: ingen sides-effect ved source, funksjonene er klare til bruk.

# ---------- Helper: hent .state sikkert ----------
get_state <- function(state = NULL) {
  if (!is.null(state)) return(state)
  s <- get0(".state", envir = .GlobalEnv, inherits = FALSE)
  if (!is.null(s)) return(s)
  stop(".state ikke funnet i global environment. Source run.R eller send state eksplisitt.")
}

# --- Robust fetch_orderbook_safe med fallback-info ---
fetch_orderbook_safe <- function(pair, state = NULL, silent = FALSE) {
  st <- tryCatch(get_state(state), error = function(e) {
    if (!silent) message("fetch_orderbook_safe: ", conditionMessage(e))
    return(structure(list(error = TRUE, msg = conditionMessage(e)), class = "orderbook_error"))
  })
  if (is.list(st) && !is.null(st$error)) return(st)
  
  res <- tryCatch({
    st$DS$orderbook(pair)
  }, error = function(e) {
    # prøv å hente body hvis tilgjengelig for feilmelding
    msg <- conditionMessage(e)
    body_txt <- NULL
    if (!is.null(e$resp) && !is.null(e$resp$body)) {
      body_txt <- tryCatch(rawToChar(e$resp$body), error = function(e2) NULL)
    }
    out <- list(error = TRUE, msg = msg)
    if (!is.null(body_txt)) out$body <- body_txt
    class(out) <- "orderbook_error"
    return(out)
  })
  
  # hvis OK, returner som før
  return(res)
}

# --- Oppdatert has_liquidity_deep med fallback til top-of-book ---
has_liquidity_deep <- function(pair,
                               need_quote,
                               side = c("buy", "sell"),
                               depth_levels = 10L,
                               safety_buffer_bps = 2,
                               state = NULL) {
  side <- match.arg(side)
  st <- tryCatch(get_state(state), error = function(e) return(structure(list(error=TRUE,msg=conditionMessage(e)), class="orderbook_error")))
  if (is.list(st) && !is.null(st$error)) return(list(ok = FALSE, reason = st$msg))
  
  ob <- fetch_orderbook_safe(pair, state = st, silent = TRUE)
  # Hvis orderbook ikke er tilgjengelig, fallback til quotes_df eller top-of-book
  if (is.list(ob) && !is.null(ob$error) && ob$error) {
    # info til debug
    reason <- ob$msg %||% "orderbook error"
    body   <- ob$body %||% NA_character_
    
    # Forsøk fallback: 1) quotes_df(pair) hvis funksjon finnes, 2) prøve q_bid/q_ask i global
    buckets_df <- NULL
    if (exists("quotes_df", envir = .GlobalEnv) && is.function(get("quotes_df", envir = .GlobalEnv))) {
      try({
        qd <- quotes_df(pair)   # forventer en liten tabell med bestBid/bestAsk
        # normalize til buckets_df med columns price, units, side
        if (is.data.frame(qd)) {
          if (all(c("bid","ask") %in% tolower(names(qd)))) {
            # hvis struktur ukjent, bygg enkel bucket fra best bid/ask
            buckets_df <- data.frame(
              price = c(as.numeric(qd$ask[1]), as.numeric(qd$bid[1])),
              units = c(as.numeric(qd$askSize[1] %||% 1), as.numeric(qd$bidSize[1] %||% 1)),
              side  = c("ASK","BID"),
              stringsAsFactors = FALSE
            )
          } else {
            # forsøke å finne relevante kolonner
            nm <- tolower(names(qd))
            # naive mapping:
            bid_col <- names(qd)[which(nm %in% c("bid","bidprice","bestbid","bid_px"))[1]]
            ask_col <- names(qd)[which(nm %in% c("ask","askprice","bestask","ask_px"))[1]]
            if (!is.null(bid_col) && !is.null(ask_col)) {
              buckets_df <- data.frame(
                price = c(as.numeric(qd[[ask_col]][1]), as.numeric(qd[[bid_col]][1])),
                units = c(1,1),
                side  = c("ASK","BID"),
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }, silent = TRUE)
    }
    
    # Hvis ikke quotes_df fallback, prøv globale q_bid/q_ask snapshot hvis de finnes
    if (is.null(buckets_df)) {
      # q_bid / q_ask i ditt miljø ser ut til å finnes fra ls()
      if (exists("q_ask", envir = .GlobalEnv) && exists("q_bid", envir = .GlobalEnv)) {
        try({
          qask <- get("q_ask", envir = .GlobalEnv)
          qbid <- get("q_bid", envir = .GlobalEnv)
          # build simple buckets if numeric
          buckets_df <- data.frame(
            price = c(as.numeric(qask), as.numeric(qbid)),
            units = c(1,1),
            side = c("ASK","BID"),
            stringsAsFactors = FALSE
          )
        }, silent = TRUE)
      }
    }
    
    # Hvis vi har buckets_df, gå videre med en én-nivås-sjekk (konservativ)
    if (!is.null(buckets_df)) {
      # velg side
      if (side == "buy") sel <- buckets_df[buckets_df$side %in% c("ASK","SELL","OFFER"), , drop = FALSE]
      else sel <- buckets_df[buckets_df$side %in% c("BID","BUY"), , drop = FALSE]
      if (nrow(sel) == 0) return(list(ok = FALSE, reason = paste0("fallback_no_side (", reason, ")"), body = body))
      # beregn enkel likviditet fra første nivå(er)
      price_i <- as.numeric(sel$price[1])
      units_i <- as.numeric(sel$units[1])
      cum_quote <- units_i * price_i
      cum_base  <- units_i
      ok <- (cum_quote >= need_quote * (1 + safety_buffer_bps/10000))
      return(list(ok = ok, cum_quote = cum_quote, cum_base = cum_base, fallback = TRUE, reason = reason, body = body))
    }
    
    # hvis ingen fallback mulig, returner opprinnelig feil
    return(list(ok = FALSE, reason = reason, body = body))
  }
  
  # --- eksisterende normalflyt når orderbook er OK ---
  buckets <- NULL
  if (!is.null(ob$orderBook) && !is.null(ob$orderBook$buckets)) buckets <- ob$orderBook$buckets
  if (is.null(buckets) && !is.null(ob$buckets)) buckets <- ob$buckets
  if (is.null(buckets) && !is.null(ob$levels)) buckets <- ob$levels
  if (is.null(buckets)) return(list(ok = FALSE, reason = "no_buckets"))
  
  bdf <- tryCatch(as.data.frame(buckets), error = function(e) return(structure(list(ok=FALSE,reason="buckets_not_df"), class="bad")))
  if (is.list(bdf) && !is.null(bdf$ok) && !bdf$ok) return(list(ok = FALSE, reason = "buckets_not_df"))
  
  # Detekter price/units kolonner (case-insensitive)
  col_names <- tolower(names(bdf))
  price_idx <- which(col_names %in% c("price","pricefloat","levelprice","rate","p"))
  units_idx <- which(col_names %in% c("units","volume","size","amount","count","qty","quantity"))
  side_idx  <- which(col_names %in% c("side","type","direction","sideIndicator"))
  
  if (length(price_idx) == 0 || length(units_idx) == 0) {
    return(list(ok = FALSE, reason = paste0("missing price/units cols: ", paste(names(bdf), collapse = ", "))))
  }
  price_col <- names(bdf)[price_idx[1]]
  units_col <- names(bdf)[units_idx[1]]
  side_col  <- if (length(side_idx)>0) names(bdf)[side_idx[1]] else NULL
  
  sel <- bdf
  if (!is.null(side_col)) {
    vals <- toupper(as.character(sel[[side_col]]))
    if (side == "buy") mask <- vals %in% c("ASK","SELL","OFFER") else mask <- vals %in% c("BID","BUY")
    sel <- sel[mask, , drop = FALSE]
  }
  if (nrow(sel) == 0) return(list(ok = FALSE, reason = "no_side_buckets"))
  
  if (side == "buy") sel <- sel[order(as.numeric(sel[[price_col]])), , drop = FALSE] else sel <- sel[order(-as.numeric(sel[[price_col]])), , drop = FALSE]
  
  s_buffer <- 1 + safety_buffer_bps / 10000
  cum_quote <- 0.0
  cum_base  <- 0.0
  levels_to_check <- min(nrow(sel), as.integer(depth_levels))
  for (i in seq_len(levels_to_check)) {
    price_i <- as.numeric(sel[[price_col]][i])
    units_i <- as.numeric(sel[[units_col]][i])
    if (is.na(price_i) || is.na(units_i)) next
    val_i_quote <- units_i * price_i
    if (cum_quote + val_i_quote >= need_quote * s_buffer) {
      remaining_quote <- need_quote * s_buffer - cum_quote
      base_from_level <- remaining_quote / price_i
      cum_base <- cum_base + base_from_level
      cum_quote <- cum_quote + remaining_quote
      break
    } else {
      cum_base <- cum_base + units_i
      cum_quote <- cum_quote + val_i_quote
    }
  }
  
  ok <- (cum_quote >= need_quote * s_buffer)
  return(list(ok = ok, cum_quote = cum_quote, cum_base = cum_base, checked_levels = levels_to_check,
              price_col = price_col, units_col = units_col))
}

# ---------- check_tri_route ----------
# route_vec: c(S, X, Y, S) e.g. c("AUD","CAD","USD","AUD")
# start_notional: beløp i start_ccy (quote for first leg)
# Merk: orientering er forenklet: vi bruker pair = paste0(from_ccy,"_",to_ccy)
check_tri_route <- function(route_vec, start_notional, start_ccy,
                            depth_levels = 10L, fee_bps = c(0,0,0), safety_buffer_bps = 2,
                            state = NULL) {
  if (length(route_vec) != 4) stop("route_vec must be length 4: c(S,X,Y,S)")
  need_quote <- as.numeric(start_notional)   # amount in current quote-currency for current leg
  current_ccy <- start_ccy
  details <- vector("list", 3)
  
  for (i in 1:3) {
    to_ccy <- route_vec[i+1]
    pair  <- paste0(current_ccy, "_", to_ccy)
    # Decide side: if we have quote currency amount and want to buy base (current_ccy is base of pair),
    # we commonly 'buy' base using quote => consume asks. This is a conservative default.
    # If your project uses a different orientation, adapt here.
    side <- "buy"
    
    res_liq <- has_liquidity_deep(pair, need_quote, side = side,
                                  depth_levels = depth_levels,
                                  safety_buffer_bps = safety_buffer_bps,
                                  state = state)
    if (!isTRUE(res_liq$ok)) {
      return(list(viable = FALSE, fail_leg = i, pair = pair, reason = res_liq$reason, detail = res_liq))
    }
    
    avg_price <- if (res_liq$cum_base > 0) res_liq$cum_quote / res_liq$cum_base else NA_real_
    base_out  <- res_liq$cum_base * (1 - ifelse(length(fee_bps) >= i, fee_bps[i], 0))
    # For next leg we need quote in next pair's quote-currency.
    # Best approximate: convert base_out back to quote using avg_price (this is a simplification)
    need_quote <- base_out * avg_price
    
    details[[i]] <- list(pair = pair, cum_quote = res_liq$cum_quote, cum_base = res_liq$cum_base,
                         avg_price = avg_price, base_out = base_out)
    current_ccy <- to_ccy
  }
  
  return(list(viable = TRUE, details = details, est_final_amount = need_quote))
}

# End of Order.R