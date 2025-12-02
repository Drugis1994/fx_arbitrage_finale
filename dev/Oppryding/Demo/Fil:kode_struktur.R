avhengigheter


C.motor.cpp  ──(Rcpp)──▶  edge_screen_cpp(), tri_pnl_cpp()

Engine_Wrappers.R  ──▶  eng_* (init/set/rebuild/mark/eval)  [valgfritt runtime-API]

utils.R
├─ build_quote_map(), make_price_matrices()      ──▶  lager M/BID/ASK for motoren
├─ edge_trigger_threshold(), eff_fee_3legs()
├─ evaluate_all() ──▶ kaller edge_screen_cpp(), tri_pnl_cpp()
└─ logging helpers

trading_bot.R
├─ route_vol_bps() (brukes i evaluate_all)
├─ spread/liq helpers
└─ execute_triangle(), kelly_size(), maybe_trade_best_triangle()

run.R
├─ setter options/konfig (fee/slip/buffer/K osv.)
├─ setter MODE, ROOT, start_ccy, volum_base
└─ sourcer filene i riktig rekkefølge