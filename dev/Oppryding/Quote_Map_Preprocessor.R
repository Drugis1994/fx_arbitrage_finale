# utils.R (engine-first minimal)
# ------------------------------
# Beholder kun små generelle helpers + trigger/fee-formler
# Quote-map og matrise-bygging er fjernet (unødvendig når C++ engine eier matrisen).

#`%||%` <- function(x, y) if (is.null(x)) y else x

#tri_dbg <- function(fmt, ...) {
 # if (isTRUE(getOption("tri.debug", FALSE))) {
    #cat(sprintf(paste0("[DBG] ", fmt, "\n"), ...))
 # }
#}

#.is_fun <- function(fn) isTRUE(exists(fn, mode = "function"))

# --- Invers quote helper (nyttig når du vil pushe begge retninger i on_tick) ---
#inv_bid_ask <- function(bid, ask) {
  #if (!is.finite(bid) || !is.finite(ask) || bid <= 0 || ask <= 0) {
    #return(c(bid = NA_real_, ask = NA_real_))
  #}
 # c(bid = 1 / ask, ask = 1 / bid)
#}

# ------- trigger/fee-formler --------------------------------------------------
#eff_fee_3legs <- function(fee_pct) {
#  if (.is_fun("eff_fee_3legs_cpp")) return(eff_fee_3legs_cpp(fee_pct))
 # 1 - (1 - fee_pct)^3
#}

#edge_trigger_threshold <- function(fee_pct, slip_frac, buffer_frac) {
 # if (.is_fun("edge_trigger_threshold_cpp")) {
  #  return(edge_trigger_threshold_cpp(fee_pct, slip_frac, buffer_frac))
  #}
  #eff_fee_3legs(fee_pct) + 3 * slip_frac + buffer_frac
#}

# ------- side_used (for logging/diagnose) ------------------------------------
# side: 0 = ask/inverse brukt, 1 = bid/direct brukt (matcher tri_pnl_cpp fallbacken din)
#side_used <- function(side, from_i, to_i, ccys, BID, ASK) {
 ## if (.is_fun("side_used_cpp")) return(side_used_cpp(side, from_i, to_i, ccys, BID, ASK))
  #if (is.na(side)) return(list(used = "none", px = NA_real_))
#  if (side == 0L) list(used = sprintf("ask %s/%s", ccys[to_i], ccys[from_i]), px = ASK[to_i, from_i])
 # else           list(used = sprintf("bid %s/%s", ccys[from_i], ccys[to_i]), px = BID[from_i, to_i])
#}