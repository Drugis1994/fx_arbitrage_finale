# ======================================================================
# logger.R — Full profesjonell logging for tri-FX-arb prosjektet
# ======================================================================

logger <- new.env(parent = emptyenv())

# ----------------------------------------------------------------------
# 1) INIT
# ----------------------------------------------------------------------
logger$init <- function(root = NULL) {
  if (is.null(root)) {
    root <- file.path(getwd(), "logs")
  }
  if (!dir.exists(root)) dir.create(root, recursive = TRUE)
  logger$root <- root
  logger$current_date <- as.character(Sys.Date())
  logger$ensure_files()
  invisible(TRUE)
}

# Opprett dagens filer hvis de mangler
logger$ensure_files <- function() {
  d <- logger$current_date
  base <- logger$root
  
  files <- c(
    engine = file.path(base, paste0(d, "_engine.csv")),
    route  = file.path(base, paste0(d, "_route.csv")),
    trade  = file.path(base, paste0(d, "_trade.csv")),
    skip   = file.path(base, paste0(d, "_skip.csv")),
    fail   = file.path(base, paste0(d, "_fail.csv")),
    error  = file.path(base, paste0(d, "_error.csv"))
  )
  
  for (f in files) {
    if (!file.exists(f)) {
      writeLines("timestamp,event,data", f)
    }
  }
  logger$files <- files
}

# ----------------------------------------------------------------------
# 2) GENERELL SKRIVEFUNKSJON
# ----------------------------------------------------------------------
logger$write <- function(type, event, data_list) {
  file <- logger$files[[type]]
  if (is.null(file)) stop("Logger type ikke funnet: ", type)
  
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S.%OS3")
  
  # Escape-kommaer / semikolon
  data_json <- jsonlite::toJSON(data_list, auto_unbox = TRUE)
  
  line <- paste(ts, event, data_json, sep = ",")
  cat(line, "\n", file = file, append = TRUE)
}

# ----------------------------------------------------------------------
# 3) LOGG-FUNKSJONER
# ----------------------------------------------------------------------

# ENGINE TOP-K EVAL
logger$log_engine <- function(res) {
  logger$write("engine", "ENGINE_EVAL", res)
}

# ROUTE-EVENT (fired i core_logger)
logger$log_route <- function(x) {
  logger$write("route", "ROUTE_EVENT", x)
}

# TRADE-EVENT
logger$log_trade <- function(x) {
  logger$write("trade", "TRADE", x)
}

# SKIP-EVENT (when liquidity fails, thresholds etc.)
logger$log_skip <- function(x) {
  logger$write("skip", "SKIP", x)
}

# FAIL-EVENT
logger$log_fail <- function(x) {
  logger$write("fail", "FAIL", x)
}

# FEILHÅNDTERING
logger$log_error <- function(msg, data = NULL) {
  logger$write("error", "ERROR", list(msg = msg, data = data))
}

# ----------------------------------------------------------------------
# 4) HJELPER FOR DAGLIG SAMMENDRAG (for analyse senere)
# ----------------------------------------------------------------------
logger$log_daily_summary <- function(df) {
  logger$write("engine", "DAILY_SUMMARY", df)
}

# ----------------------------------------------------------------------
# 5) AUTOINIT PÅ LOAD
# ----------------------------------------------------------------------
logger$init()