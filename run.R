# Run.R

# --- Libraries ---
suppressPackageStartupMessages({
  library(Rcpp); library(httr2); library(jsonlite)
  library(dplyr); library(lubridate); library(readr)
  library(curl);  library(rlang);     library(tidyr); library(dotenv)
})

# --- Reset debug state ---
suppressWarnings({
  options(error = NULL)
  debuggingState(FALSE)
  try(undebug(base::open.connection), silent = TRUE)
  try(undebug(base::readBin),        silent = TRUE)
  try(untrace(base::open.connection), silent = TRUE)
  try(untrace(base::readBin),         silent = TRUE)
})

# --- Project root ---
Sys.setenv(CURL_DNS_SERVERS = "1.1.1.1,8.8.8.8")
setwd("/Users/sigurdberner/FX_ARBITRAGE_FINALE copy")
ROOT <- "/Users/sigurdberner/FX_ARBITRAGE_FINALE copy"
readRenviron(file.path(ROOT, ".env"))

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Tuning knobs ---
options(
  arb.matrix_tol           = 1e-12,
  arb.K                    = 300L,
  arb.fee_pct              = 0,
  arb.assumed_slip_bps     = 0,
  arb.safety_buffer_bps    = 0,
  arb.k_slip               = 0,
  arb.rvbps_fallback_bps   = 0,
  arb.near_window_bps      = 0,
  arb.min_interval_ms      = 0L,
  arb.min_interval_fast_ms = 1L,
  arb.start_ccy            = "USD",
  arb.bankroll             = 100000,
  arb.bps_scale            = 1e4,
  arb.min_edge_bps         = 0L
)

# --- Load core files ---
source(file.path(ROOT, "R/core_runtime.R"))
source(file.path(ROOT, "R/DirectStream.R"))
source(file.path(ROOT, "R/Trading_Bot.R"))
source(file.path(ROOT, "R/Engine_Wrapper.R"))

# --- Load C++ engine (minimal) ---
engine_load <- function(path = file.path(ROOT, "C++", "libengine_v4.dylib")) {
  p <- normalizePath(path, mustWork = TRUE)
  if (!is.loaded("engine_poll_R")) dyn.load(p)
  invisible(TRUE)
}
engine_load()

message("✅ All core scripts loaded.")

# --- Bootstrap ---
stopifnot(DS$has_auth())
cat("🚀 Oppsett klart…\n")


all_instr <- DS$list_symbols()
STREAM_UNIVERSE <- (100L)

cat(sprintf("\n📦 Abonnerer på %d instrumenter:\n%s\n",
            length(STREAM_UNIVERSE),
            paste(STREAM_UNIVERSE, collapse = ", ")))

# --- Build state ---
state <- init_state(
  DS          = DS,
  instruments = STREAM_UNIVERSE,
  start_ccy   = getOption("arb.start_ccy")
)

# --------------------------------------------------------
# ⭐⭐⭐ INIT ENGINE EXACTLY HERE ⭐⭐⭐
# --------------------------------------------------------
start_ccy <- getOption("arb.start_ccy")
engine_init(state, start_ccy)
# --------------------------------------------------------

# --- Tick handler ---
on_tick <- make_on_tick(state)


# =====================================================================
# 📊 PRINT RESULTATER + BEN-FOR-BEN DEBUGGING
# =====================================================================
poll_and_print <- function(state) {
  res <- engine_poll(state$eng)
  
  if (is.null(res) || nrow(res) == 0) {
    cat("❕ Ingen ruter funnet i denne ticken\n")
    return(invisible())
  }
  
  cat("──────────────────────────────────────────────\n")
  cat("📊 TRI-ARBITRAGE RESULTATER (", nrow(res), " ruter )\n", sep="")
  
  for (k in seq_len(nrow(res))) {
    r <- res[k, ]
    
    cat(sprintf(
      "• route_id=%d | mid=%s | end=%s | pnl=%.5f | edge=%.5f | pct=%.3f%% | ns=%s\n",
      r$route_id, r$mid, r$end, r$pnl, r$edge, r$pct*100, format(ns(), scientific = FALSE)
    ))
  }
}


# =====================================================================
# 🔄 WRAPPED TICK
# =====================================================================
wrapped_on_tick <- function(sym, bid, ask, time) {
  
  if (identical(sym, "HEARTBEAT")) return(invisible())
  
  on_tick(sym, bid, ask, time)
  
  poll_and_print(state)
}


# =====================================================================
# 🔴 START STREAM
# =====================================================================
cat("\n🔴 Starter LIVE stream… (Ctrl+C for å stoppe)\n")

DS$stream_prices(
  instruments        = STREAM_UNIVERSE,
  on_tick            = wrapped_on_tick,
  include_heartbeats = TRUE,
  verbose            = TRUE,
  snapshot           = TRUE
)

cat("✅ Ferdig.\n")







