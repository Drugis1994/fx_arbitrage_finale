# --- Libraries ---
suppressPackageStartupMessages({
  library(Rcpp); library(httr2); library(jsonlite)
  library(dplyr); library(lubridate); library(readr)
  library(curl);  library(rlang);     library(tidyr); library(dotenv); library(BH); library(cpp11); library(Rcpp);

})

## Project root
Sys.setenv(CURL_DNS_SERVERS = "1.1.1.1,8.8.8.8")
setwd("~/Documents/FX_Ferdig_project")
ROOT <- getwd()
readRenviron(file.path(ROOT, ".env"))

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Global defaults (overstyr i .Rprofile / .Renviron / options(...) før run) ---
options(
         # ========= [GENERELT / MOTOR] =========
         ## settings.R (legg eller oppdater disse)

  arb.K                    = 300L,     # Hvor mange trekanter (Top-K) motoren vurderer per evaluering
  arb.fee_pct              = 0.000,    # 1 bp per legg – mer realistisk enn 0 i produksjon
  arb.assumed_slip_bps     = 0,        # Liten basis-slippage i bps
  arb.k_slip               = 0.30,     # Koeffisient for vol-basert slippage (k * rvbps)
  arb.rvbps_fallback_bps   = 10.0,     # Fallback-volatilitet i bps hvis rvbps ikke kan estimeres
  arb.near_window_bps      = 5,        # «Near»-vindu i bps; innenfor dette flagges near=TRUE
  arb.min_interval_ms      = 5L,       # Min. millisekunder mellom normale evalueringer (throttle)
  arb.min_interval_fast_ms = 1L,       # Min. ms for «fast path» (når noe er «near»)
  arb.start_ccy            = "USD",    # Start-/slutt-valuta for trekantene (bankroll denomineres i denne)
  arb.bankroll             = 100000,   # Startmengde i startvaluta som brukes i simulering/ordre
  arb.bps_scale            = 1e4,      # Skaleringsfaktor for bps (1e4 => 1 bps = 1/100 av 1%)
  arb.matrix_tol           = 1e-12,    # Numerisk toleranse for matrix-diff i motoren
  
  # ========= [RISIKO / HANDEL] =========
  arb.min_edge_bps         = 0L,       # Minste edge i bps som kreves før en handel vurderes
  arb.cooldown_sec         = 0.0,      # Litt ekstra «pust» mellom handler
  arb.dry_run              = FALSE,    # FALSE = live-ordre er på; TRUE = kun simulering/logg (ingen ordre)
  arb.frac_kelly           = 0.0002,   # Andel av Kelly-størrelse som faktisk brukes (risikoskalering)
  arb.max_notional_start   = 2500,     # Strammere cap pr. trade for rolig produksjon
  arb.min_units            = 1L,       # Minste antall enheter per ordre (meglerkrav/avrunding)
  arb.kelly_var_floor      = 1e-12,    # Varians-gulv i Kelly-beregning for å unngå eksploderende sizing
  arb.bankroll_default     = 10000,    # Fallback-bankroll hvis ikke oppgitt annet sted
  
  # ========= [SPREADS & CAPS / LIKVIDITET] =========
  arb.spread_default_cap_bps = 400,     # Kast bort veldig brede quotes
  arb.spread_cap             = NULL,   # Evt. per-ccy caps i en named list, f.eks. list(JPY=25, TRY=80)
  arb.liq.min_notional       = 20,     # Minimum notional for rask likviditets-sjekk
  arb.liq.depth_levels       = 3L,     # Hvor mange nivåer i orderbok vi aksepterer som «likvid»
  arb.liq.cache_ttl_ms       = 300L,   # Cache TTL for orderbok-sjekk (ms)
  
  # ========= [PRINT / LOGGING – BASIS] =========
  arb.topK.print                     = 25L,    # Antall ruter å skrive ut per batch (før evt. override nedenfor)
  arb.print.core_verbose             = TRUE,   # TRUE = detaljerte rute-linjer via core-logger
  arb.print.core_only_edge_or_profit = FALSE,  # TRUE = skriv bare ruter med edge>=trigger ELLER PnL>0
  arb.print.profit_only              = FALSE,  # TRUE = skriv bare ruter med positiv Net PnL
  arb.print.core_summary             = TRUE,   # TRUE = skriv oppsummering (f.eks. per tick)
  arb.print.core_heartbeat           = FALSE,  # TRUE = skriv «.» på heartbeats (basisverdi; se override nedenfor)
  
  # ========= [STREAM / PROXY / OANDA] =========
  arb.user_agent         = "tri-fx-arb/0.1 (R httr2)",  # UA for REST-kall
  arb.user_agent_stream  = "tri-fx-arb/0.1 (R curl)",   # UA for stream-kall
  oanda.http_timeout     = 30,        # HTTP-timeout i sekunder per kall
  oanda.http_max_tries   = 4,         # Maks antall forsøk ved transient feil
  oanda.http_backoff_min = 0.4,       # Min. backoff-intervall mellom forsøk (sekunder)
  oanda.http_backoff_max = 1.2,       # Maks backoff-intervall mellom forsøk (sekunder)
  oanda.stream.accept_encoding = "identity",  # Encoding-policy for stream (u/ gzip)
  oanda.stream.max_instruments = 50L,         # Maks antall instrumenter i stream-abonnementet
  
  # ========= [STREAM-FLAGG] =========
  arb.stream.include_heartbeats = TRUE,   # Inkluder heartbeats i stream-callback
  arb.stream.verbose            = TRUE,  # Detaljert logging fra datastrøm (ikke motor-logging)
  arb.stream.snapshot           = TRUE,  # Hent snapshot før stream (her: av)
  
  # ========= [MOTOR / TOP-K-UTTAK] =========
  arb.engine.prefer_profit_first = FALSE, # FALSE = ikke bias mot «profit first» i utvalg av ruter
  
  # ========= [PRINT / LOGGING – OVERRIDES / DEBUG] =========
  arb.print.only_when_viable       = FALSE,  # FALSE = ikke gate printing; skriv også «ikke-viable» (støy)
  arb.print.core_verbose           = TRUE,   # (override) behold detaljert rute-logging
  arb.print.core_only_edge_or_profit = FALSE,# (override) skriv også ruter uten edge/PNL (for testing)
  arb.print.profit_only            = FALSE,  # (override) ikke profit-only
  arb.engine.prefer_profit_first   = FALSE,  # (override) samme som over – tydelighet
  arb.topK.print                   = 60L,    # Mer nøkternt enn 999L – mindre spam, fortsatt god innsikt
  arb.print.core_summary           = TRUE,   # (override) behold oppsummering
  arb.print.core_heartbeat         = TRUE   # (override) hold heartbeats stille for renere logger
  
  # ========= [VALGFRIE «TOGGLES» DU KAN SLÅ PÅ VED BEHOV] =========
  # arb.near_window_bps              = 1000,   # Debug: 10% «near»-vindu for masse utskrift
  # arb.fee_pct                      = 0,      # Debug: fjern gebyr for lavere trigger
  # arb.assumed_slip_bps             = 0,      # Debug: fjern basisslippage
  # arb.k_slip                       = 0,      # Debug: fjern vol-slippage
  # arb.safety_buffer_bps            = 0,      # Debug: fjern sikkerhetsmargin
  # arb.K                            = 2000L,  # Debug: søk i mye større Top-K-rom
  # arb.min_interval_ms              = 0L,     # Debug: ingen throttling (kan spamme CPU)
  # arb.min_interval_fast_ms         = 0L      # Debug: ingen throttling på «fast path»
)
arb.safety_buffer_bps <- getOption("arb.liq.safety_buffer_bps")
# --- Formatters (produserer tekst) ---
options(
  arb.fmt.route = function(x) {
    sprintf(
      "%s %s -> %s -> %s | Final %s: %.2f | Net PnL: %.2f (%.3f%%) | Edge(before cost): %s | Trigger: %.3f%% | %s | legs: [%s | %s | %s] | ⏱ %.2f ms\n",
      x$tag %||% "ℹ️",
      x$route$start, x$route$mid, x$route$end, x$route$start,
      x$final_base %||% NA_real_,
      x$netPnL %||% NA_real_,
      x$pnl_pct %||% NA_real_,
      if (is.finite(x$edge_before_cost)) sprintf("%.3f%%", 100 * x$edge_before_cost) else "NA",
      x$trigger_pct %||% NA_real_,
      if (isTRUE(x$near)) "near=TRUE" else "near=FALSE",
      (x$legs %||% c(NA, NA, NA))[1],
      (x$legs %||% c(NA, NA, NA))[2],
      (x$legs %||% c(NA, NA, NA))[3],
      x$took_ms %||% NA_real_
    )
  },
  arb.fmt.summary = function(x) {
    sprintf("⏱ Routes: %d | Avg: %.2f ms | Max: %.2f ms\n",
            x$routes %||% 0L, x$avg_ms %||% 0, x$max_ms %||% 0)
  },
  arb.fmt.no_profit = function(x) {
    "❌ Ingen lønnsom arbitrasje (Net PnL > 0) i denne oppdateringen.\n"
  },
  # ========== Nytt: formatters for trade-events ==========
  arb.fmt.trade = function(p) {
    sprintf(
      "✅ TRADE %s -> %s -> %s -> %s | edge=%.3f bps | vol≈%.2f bps | size=%.0f %s | eval=%.3fs exec=%.3fs | dry_run=%s\n",
      (p$route %||% c(NA,NA,NA,NA))[1], (p$route %||% c(NA,NA,NA,NA))[2],
      (p$route %||% c(NA,NA,NA,NA))[3], (p$route %||% c(NA,NA,NA,NA))[4],
      as.numeric(p$dyn_edge_bps %||% NA_real_),
      as.numeric(p$vol_bps %||% NA_real_),
      as.numeric(p$size_start %||% NA_real_),
      p$start_ccy %||% "",
      as.numeric(p$time_eval_s %||% NA_real_),
      as.numeric(p$time_exec_s %||% NA_real_),
      as.character(p$dry_run %||% NA)
    )
  },
  arb.fmt.trade_dryrun = function(p) {
    legs <- p$legs %||% list()
    leg_txt <- paste(vapply(legs, function(l)
      sprintf("%s %s %s", l$side %||% "?", l$units %||% "?", l$instrument %||% "?"), ""), collapse = "  |  ")
    sprintf("🧪 DRYRUN %s -> %s -> %s -> %s | %s\n",
            (p$route %||% c(NA,NA,NA,NA))[1], (p$route %||% c(NA,NA,NA,NA))[2],
            (p$route %||% c(NA,NA,NA,NA))[3], (p$route %||% c(NA,NA,NA,NA))[4],
            leg_txt)
  },
  arb.fmt.trade_skip = function(p) {
    sprintf("⏭️  SKIP (%s) %s -> %s -> %s -> %s\n",
            p$reason %||% "unknown",
            (p$route %||% c(NA,NA,NA,NA))[1], (p$route %||% c(NA,NA,NA,NA))[2],
            (p$route %||% c(NA,NA,NA,NA))[3], (p$route %||% c(NA,NA,NA,NA))[4])
  },
  arb.fmt.trade_fail = function(p) {
    sprintf("❌ FAIL [%s] %s -> %s -> %s -> %s | err=%s\n",
            p$stage %||% p$result %||% "unknown",
            (p$route %||% c(NA,NA,NA,NA))[1], (p$route %||% c(NA,NA,NA,NA))[2],
            (p$route %||% c(NA,NA,NA,NA))[3], (p$route %||% c(NA,NA,NA,NA))[4],
            p$error %||% "n/a")
  }
)

# --- Unified logger (kan kobles til CSV etc. senere) ---
options(
  arb.core_logger = function(event, payload) {
    v    <- isTRUE(getOption("arb.print.core_verbose", TRUE))
    only <- isTRUE(getOption("arb.print.core_only_edge_or_profit", FALSE))
    sumr <- isTRUE(getOption("arb.print.core_summary", TRUE))
    hb   <- isTRUE(getOption("arb.print.core_heartbeat", FALSE))
    
    if (identical(event, "tick")) {
      if (hb) cat(".")
      return(invisible())
    }
    if (identical(event, "route")) {
      if (!v) return(invisible())
      if (only && !(isTRUE(payload$is_profit) || isTRUE(payload$is_edge))) return(invisible())
      fmt <- getOption("arb.fmt.route", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    if (identical(event, "summary")) {
      if (!sumr) return(invisible())
      fmt <- getOption("arb.fmt.summary", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    if (identical(event, "no_profit")) {
      if (!v) return(invisible())
      fmt <- getOption("arb.fmt.no_profit", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    # ======== Nytt: trade-events fra trading_bot.R ========
    if (identical(event, "trade")) {
      fmt <- getOption("arb.fmt.trade", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    if (identical(event, "trade_dryrun")) {
      fmt <- getOption("arb.fmt.trade_dryrun", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    if (identical(event, "trade_skip")) {
      fmt <- getOption("arb.fmt.trade_skip", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    if (identical(event, "trade_fail")) {
      fmt <- getOption("arb.fmt.trade_fail", NULL)
      txt <- if (is.function(fmt)) tryCatch(fmt(payload), error = function(e) "") else ""
      if (nzchar(txt)) cat(txt)
      return(invisible())
    }
    invisible()
  }
)



# --- Core R files ---
source(file.path(ROOT, "R", "DS.R"))
source(file.path(ROOT, "R", "utils.R"))
source(file.path(ROOT, "R", "tradingrobot.R"))
source(file.path(ROOT, "R", "Engine_Wrappers.R"))
Rcpp::sourceCpp(file.path(ROOT, "R", "Ccomputing.cpp"))
source(file.path(ROOT, "R", "core_runtime.R"))
suppressWarnings({ if (file.exists(file.path(ROOT, "R", "Dev_utils.R"))) source(file.path(ROOT, "R", "Dev_utils.R")) })
# --- C++ engine først ---
Rcpp::sourceCpp(file.path(ROOT, "R", "traderhelperC.cpp"))    # ← NY (dine preselect/tri_pnl_batch m.m.)
message("✅ All core and optional scripts sourced successfully.")
## ---- Bootstrap & stream ----
stopifnot(DS$has_auth())

cat("🚀 Oppsett klart…\n")
all_instr <- DS$list_symbols()
STREAM_UNIVERSE <- head(sort(all_instr), min(length(all_instr), getOption("oanda.stream.max_instruments", 50L)))
cat(sprintf("\n📦 Abonnerer på %d instrumenter:\n%s\n",
            length(STREAM_UNIVERSE), paste(STREAM_UNIVERSE, collapse = ", ")))

state <- init_state_and_engine(
  
  ROOT        = ROOT,
  DS          = DS,
  instruments = STREAM_UNIVERSE,
  start_ccy   = getOption("arb.start_ccy")
)

# Bygg on_tick fra core (styrt av options)
on_tick <- make_on_tick(
  state,
  verbose = getOption("arb.print.core_verbose")
)

cat("\n🔴 Starter LIVE stream… (Ctrl+C for å stoppe)\n")
DS$stream_prices(
  instruments        = STREAM_UNIVERSE,
  on_tick            = on_tick,
  include_heartbeats = getOption("arb.stream.include_heartbeats", TRUE),
  verbose            = getOption("arb.stream.verbose", TRUE),
  snapshot           = getOption("arb.stream.snapshot", TRUE)
)

cat("✅ Ferdig.\n")
