# streaming.R — intentionally no custom streaming client.
# Streaming is handled centrally via DS$stream_prices(...) from DS.R.
# This file only provides a guard to prevent accidental use of a legacy start_stream().

# ---- Guard: fail fast if old code tries to call start_stream() ----
start_stream <- function(...) {
  stop(
    paste(
      "start_stream() has been removed.",
      "Use DS$stream_prices(instruments = STREAM_UNIVERSE, on_tick = on_tick,",
      "                     include_heartbeats = TRUE, verbose = TRUE, snapshot = TRUE)",
      "from your run.R instead.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

# ---- (Optional) Small helpers for diagnostics; safe no-ops if unused ----

# Print a compact view of current quotes in state$latest
stream_print_quotes <- function(state, n = 10L) {
  if (is.null(state) || is.null(state$latest)) {
    cat("ℹ️  No state/quotes available yet.\n"); return(invisible(NULL))
  }
  keys <- ls(state$latest, all.names = FALSE)
  if (!length(keys)) { cat("ℹ️  No quotes in state.\n"); return(invisible(NULL)) }
  keys <- head(sort(keys), n)
  for (k in keys) {
    q <- state$latest[[k]]
    if (is.list(q) && is.finite(q$bid) && is.finite(q$ask)) {
      cat(sprintf("%-9s  bid=%-12g ask=%-12g  time=%s\n", k, q$bid, q$ask, q$time %||% ""))
    }
  }
  invisible(NULL)
}

# Check for stale quotes older than max_age_ms
stream_check_stale <- function(state, max_age_ms = 1000) {
  if (is.null(state) || is.null(state$latest)) return(invisible(character()))
  now <- as.numeric(Sys.time()) * 1000
  stale <- character()
  for (k in ls(state$latest)) {
    q <- state$latest[[k]]
    tt <- q$time
    if (inherits(tt, "POSIXct")) {
      age <- abs(as.numeric(tt) * 1000 - now)
    } else if (is.character(tt) && nzchar(tt)) {
      # Try to parse RFC3339 / OANDA-like
      ts <- try(as.POSIXct(tt, tz = "UTC"), silent = TRUE)
      if (inherits(ts, "try-error") || is.na(ts)) next
      age <- abs(as.numeric(ts) * 1000 - now)
    } else {
      next
    }
    if (is.finite(age) && age > max_age_ms) stale <- c(stale, sprintf("%s(%dms)", k, round(age)))
  }
  if (length(stale)) {
    cat("⚠️  Stale quotes:", paste(stale, collapse = " , "), "\n")
  } else {
    cat("✅ No stale quotes.\n")
  }
  invisible(stale)
}

# NULL-coalescing used above; keeps file self-contained
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x