#SELG ALLLE POSISJONER/CLOSE
emergency_flat_all()


###optimlasere computing tid
library(microbenchmark)

microbenchmark(
  edge_screen = edge_screen(.state),          # raskeste del av koden
  tri_pnl     = tri_arbitrage_pnl(.state),    # full beregning
  mids_map    = mids_matrix(.state),          # matriseoppbygging
  times = 50
)
## ====================== SETUP (trygg bootstrap) ======================
# Forutsetter at run.R/core_runtime.R/Engine_Wrappers.R osv. er sourcet
stopifnot(exists("state", inherits = TRUE), is.environment(state))
stopifnot(is.matrix(state$M), is.matrix(state$BID), is.matrix(state$ASK))

# 1) Sørg for route_idx
if (is.null(state$route_idx)) {
  if (exists("precompute_route_idx")) {
    state$route_idx <- precompute_route_idx(state$ccys, getOption("arb.start_ccy"))
  } else if (exists("build_route_idx")) {
    state$route_idx <- build_route_idx(state$ccys, getOption("arb.start_ccy"))
  } else stop("Mangler build_route_idx()/precompute_route_idx()")
}

# 2) Sørg for CAP-matrise (spread caps)
if (is.null(state$CAP) && exists(".init_cap_matrix")) .init_cap_matrix(state)

# 3) Indeks for startvaluta
s_i <- match(getOption("arb.start_ccy",), state$ccys); stopifnot(!is.na(s_i))

## ====================== MATRISER: INSPEKSJON ======================
# Dimensjoner og topp-utsnitt
dim(state$M); dim(state$BID); dim(state$ASK)
head(state$M[, 1:min(5, ncol(state$M))])
head(state$BID[, 1:min(5, ncol(state$BID))])
head(state$ASK[, 1:min(5, ncol(state$ASK))])

# Finite/NA-statistikk
sum(is.finite(state$M)); sum(!is.finite(state$M))
sum(is.finite(state$BID)); sum(!is.finite(state$BID))
sum(is.finite(state$ASK)); sum(!is.finite(state$ASK))

# Sammendrag (kan være stort)
summary(as.vector(state$M))
summary(as.vector(state$BID))
summary(as.vector(state$ASK))

# CAP-oversikt (maks spredder i bps)
if (!is.null(state$CAP)) summary(as.vector(state$CAP))

# Kikk på konkrete par (bytt "EUR","USD" etc. etter behov)
state$BID["EUR","USD"]; state$ASK["EUR","USD"]
state$BID["USD","JPY"]; state$ASK["USD","JPY"]

## ====================== ROUTE-INDEX (mid_i,end_i) ======================
dim(state$route_idx)
head(state$route_idx, 10)

## ====================== EDGE-SCREEN (C++ / R-fallback) ======================
# Kjør screening på hele route_idx
es <- edge_screen_cpp(s_i, state$route_idx, state$M)
head(es, 10)
summary(es$edge)
sum(is.finite(es$edge))

# Topp-10 ruter etter edge (før kost)
ord <- order(-es$edge %||% rep(-Inf, nrow(es)))
head(cbind(es[ord, ][, c("mid_i","end_i","edge")], 
           mid=state$ccys[es$mid_i[ord]], end=state$ccys[es$end_i[ord]]), 10)

## ====================== TRI-PNL (C++ / R-fallback) ======================
# Test PnL på én rute (første rad)
ri1 <- matrix(state$route_idx[1, ], 1, 2)
tri_pnl_cpp(s_i, ri1, state$BID, state$ASK, start_qty_base = 10000, fee_pct = getOption("arb.fee_pct", 0))

# Batch: første 5 ruter
ri5 <- state$route_idx[1:5, , drop = FALSE]
tri_pnl_cpp(s_i, ri5, state$BID, state$ASK, start_qty_base = 10000, fee_pct = getOption("arb.fee_pct", 0))

## ====================== SPREAD & VOLATILITET ======================
# Spread i bps med cap-gating (Inf betyr “for bred/ikke tilgjengelig”)
pair_spread_bps2("EUR","USD", state = state)
pair_spread_bps2("GBP","JPY", state = state)
pair_spread_bps2("NZD","AUD", state = state)

# Varm opp vol-bufferen fra 'latest' (om ikke gjort)
if (exists("push_mid")) {
  for (p in names(state$latest)) {
    b <- as.numeric(state$latest[[p]]$bid); a <- as.numeric(state$latest[[p]]$ask)
    if (is.finite(b) && is.finite(a)) push_mid(p, 0.5*(a+b))
  }
}
# Vol i bps for én route (3 par)
if (exists("route_vol_bps")) {
  route_pairs <- c("USD/EUR","EUR/GBP","GBP/USD")
  route_vol_bps(route_pairs)
}

## ====================== ENGINE-EVAL (TOP-K) ======================
# Direkte kall til engine_eval_topK via wrapperen
etk <- engine_eval_topK(
  K                  = getOption("arb.K", 200L),
  fee_pct            = getOption("arb.fee_pct", 0),
  slip_base_frac     = getOption("arb.assumed_slip_bps", 0)/1e4,
  buf_base_frac      = getOption("arb.safety_buffer_bps", 2)/1e4,
  k_slip             = getOption("arb.k_slip", 0.30),
  rvbps_fallback_bps = getOption("arb.rvbps_fallback_bps", 10.0),
  start_qty_base     = getOption("arb.bankroll", 100000)
)
str(etk)

# Kjør en tick i motoren (uten stream), nyttig for timing
t0 <- Sys.time()
out <- engine_tick_once(
  state$eng,
  K                    = getOption("arb.K", 200L),
  fee_pct              = getOption("arb.fee_pct", 0),
  slip_base_frac       = getOption("arb.assumed_slip_bps", 0)/1e4,
  buf_base_frac        = getOption("arb.safety_buffer_bps", 2)/1e4,
  k_slip               = getOption("arb.k_slip", 0.30),
  rvbps_fallback_bps   = getOption("arb.rvbps_fallback_bps", 10.0),
  start_qty_base       = getOption("arb.bankroll", 100000),
  near_window_frac     = getOption("arb.near_window_bps", 2)/1e4,
  now_ms               = as.numeric(Sys.time()) * 1000,
  min_interval_ms      = getOption("arb.min_interval_ms", 5L),
  min_interval_fast_ms = getOption("arb.min_interval_fast_ms", 1L)
)
t_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
cat(sprintf("engine_tick_once(): %.3f ms | ran=%s | near=%s | n=%d\n",
            t_ms, out$ran, out$near, length(out$result$idx %||% integer(0))))

## ====================== MICROBENCHMARKS ======================
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  library(microbenchmark)
  ri1 <- matrix(state$route_idx[1, ], 1, 2)
  mb <- microbenchmark(
    edge_screen = edge_screen_cpp(s_i, state$route_idx, state$M),
    tri_pnl     = tri_pnl_cpp(s_i, ri1, state$BID, state$ASK, 10000, getOption("arb.fee_pct", 0)),
    times = 100L
  )
  print(mb); summary(mb)
}

## ====================== SANITY: ENKEL PNL/EDGE FOR EN ROUTE ======================
# Bygg én konkret tri (S->X->Y->S) fra indeksene i route_idx
one <- state$route_idx[1, ]
S <- state$ccys[s_i]; X <- state$ccys[one[1]]; Y <- state$ccys[one[2]]
cat(sprintf("Route: %s -> %s -> %s -> %s\n", S, X, Y, S))

# Edge (før kostnader) for denne
vmid <- state$M[s_i, one[1]]; mmid <- state$M[one[1], one[2]]; dmid <- state$M[s_i, one[2]]
edge_one <- if (all(is.finite(c(vmid, mmid, dmid))) && dmid > 0) (vmid*mmid)/dmid - 1 else NA_real_
edge_one

# PnL (inkl. bid/ask + fee)
tri_pnl_cpp(s_i, matrix(one,1,2), state$BID, state$ASK, start_qty_base = 10000, fee_pct = getOption("arb.fee_pct", 0))


















###TEST ORDRE
# --- helpers/test_trade.R ---------------------------------------------
test_trade <- function(instr = "EUR_USD", units = 1000L, side = c("buy","sell"),
                       wait_sec = 0.25, ds = DS) {
  side <- match.arg(side)
  # 1) open
  open <- try(ds$place_market(instrument = instr, side = side, units = units), silent = TRUE)
  if (inherits(open, "try-error")) stop("open failed: ", conditionMessage(attr(open, "condition")))
  ofill <- open$orderFillTransaction
  open_px <- as.numeric(ofill$price)
  open_id <- ofill$tradeOpened$tradeID %||% ofill$tradesOpened[[1]]$tradeID %||% NA
  
  # 2) wait a tick (optional)
  if (wait_sec > 0) Sys.sleep(wait_sec)
  
  # 3) close (opposite side)
  side2 <- if (side == "buy") "sell" else "buy"
  close <- try(ds$place_market(instrument = instr, side = side2, units = units), silent = TRUE)
  if (inherits(close, "try-error")) stop("close failed: ", conditionMessage(attr(close, "condition")))
  cfill <- close$orderFillTransaction
  close_px <- as.numeric(cfill$price)
  
  # 4) summarize
  quotePL <- as.numeric(cfill$quotePL %||% 0)
  homePL  <- as.numeric(cfill$pl %||% 0)
  
  list(
    instrument = instr,
    units      = units,
    open_side  = side,
    open_px    = open_px,
    close_px   = close_px,
    spread_px  = close_px - open_px,          # negative if you cross spread
    quotePL    = quotePL,                      # in quote currency (USD for EUR_USD)
    homePL     = homePL,                       # account home CCY
    open_id    = open_id,
    raw_open   = open,
    raw_close  = close
  )
}

# Ta noen symboler (du kan bruke hele universe også)
syms <- DS$list_symbols()
snap <- DS$snapshot(head(syms, 50))  # ta gjerne flere enn 5
# examples
analyze_log_routes()   # rich per-route view
analyze_log_daily()    # daily aggregation

# Hydrer runtime-state med bid/ask i "BASE/QUOTE"-format
for (i in seq_len(nrow(snap))) {
  pair <- snap$pair[i]                 # allerede "AUD/CAD"-stil
  .state$latest[[pair]] <- list(
    bid = snap$bid[i],
    ask = snap$ask[i]
  )
}

# Kjør én evaluering for å bekrefte at alt henger sammen
evaluate_all(.state)





#⚙️ DS (Datasource-adapter / OANDA-modulen)
#1 Test at API-nøkkelen fungerer

DS$ping()    

# Hent tilgjengelige valutapar (kort liste over symbols)
head(DS$list_symbols(), 20)

# Hent siste candles for et par
DS$fetch_history(c("EUR_USD"), granularity = "M1", count = 5)

# Hent flere par samtidig (f.eks. NOK-relaterte)
DS$fetch_history(c("USD_NOK", "EUR_NOK"), granularity = "M5", count = 10)

# Lagre resultatet i en variabel for videre bruk
data <- DS$fetch_history(c("EUR_USD","USD_NOK"), "M1", 10)
head(data)







#📊 Runtime-state / Quotes-inspeksjon

ls(.state$latest)        # Se hvilke valutapar som er i minnet
quotes_df()              # Vis alle aktive quotes (bid/ask/mid/time)
qm <- build_quote_map(as.list.environment(.state$latest))
ls(qm)                   # Alle tilgjengelige par i “matrisen”
qm[["EUR/NOK"]]          # Direkte quote
qm[["NOK/EUR"]]          # Invers (automatisk generert)


#🔺 Diagnose & arbitrasjeanalyse
#diagnose_triangles()     # Bygger tabellen triangle_diagnose
head(triangle_diagnose)  # Vis de første radene
nrow(triangle_diagnose)  # Totalt antall ruter
sum(triangle_diagnose$status != "OK")  # Hvor mange mangler noe

# Evaluering av alle ruter med logg
evaluate_all()  

#assumed_slip_bps  <- 0.0003   # 3 bps
#safety_buffer_bps <- 0.0002   # 2 bps
# fee_pct can stay 0 in research; in live set a realistic per-leg fee
#Focus the universe on the tightest-spread majors to increase the chance that a mid-edge survives bid/ask
#universe <- build_universe(DS$list_symbols(), prefer = c("USD","EUR","GBP","JPY","CHF","AUD","CAD","NZD","SEK","DKK"))

# Vis toppkandidater (sortert på edge eller PnL)
show_top_candidates(metric = "pnl")   
show_top_candidates(metric = "edge")

#💾 Logging og historikk

# Se siste 10 linjer i loggfilen
readr::read_csv("data/arbitrasje_logg.csv", show_col_types = FALSE) |> tail(10)

# Analyser loggene samlet (krever analyze_log-funksjonen)
resultater <- analyze_log()
resultater$summary     # Aggregerte resultater per rute



#replay_history(DS, instruments = c("EUR_NOK","USD_NOK","EUR_USD"), granularity = "M1", count = 15, sleep_secs = 0)

# Begrens til kun 10 ticks for rask test
#replay_history(DS, c("EUR_NOK","USD_NOK","EUR_USD"), count = 10, max_ticks = 10)