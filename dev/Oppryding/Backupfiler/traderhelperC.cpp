#include <Rcpp.h>
using namespace Rcpp;
// trading_bot_cpp_helpers.cpp
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(Rcpp)]]

#include <algorithm>
#include <vector>
#include <cmath>

static inline bool finite_pos(double x){ return std::isfinite(x) && x > 0.0; }

static inline double eff_fee_3legs(double f){
  // 1 - (1 - f)^3
  return 1.0 - (1.0 - f)*(1.0 - f)*(1.0 - f);
}

// ------------------------------------------------------------------
// 1) Spread-caps: filter alle kandidater i ett kall
// [[Rcpp::export]]
LogicalVector filter_candidates_cpp(
    IntegerVector mid_i,   // 1-basert
    IntegerVector end_i,   // 1-basert
    NumericMatrix BID,
    NumericMatrix ASK,
    NumericMatrix CAP,     // cap i bps (NA = ubegrenset)
    int start_i_1b         // 1-basert
){
  int n = mid_i.size(), s0 = start_i_1b - 1;
  LogicalVector keep(n, false);
  
  auto ok = [&](int i0, int j0)->bool{
    double bid = BID(i0,j0), ask = ASK(i0,j0);
    if (!finite_pos(bid) || !finite_pos(ask)) return false;
    double mid = 0.5*(bid+ask); if (!finite_pos(mid)) return false;
    double bps = (ask - bid)/mid*1e4;
    double cap = CAP(i0,j0);
    // NA cap = ingen begrensning
    return !std::isfinite(cap) || bps <= cap;
  };
  
  for (int k=0; k<n; ++k){
    int m0 = mid_i[k]-1, e0 = end_i[k]-1;
    if (m0<0 || e0<0) continue;
    if (!ok(s0,m0)) continue;
    if (!ok(m0,e0)) continue;
    if (!ok(e0,s0)) continue;
    keep[k] = true;
  }
  return keep;
}

// ------------------------------------------------------------------
// 2) Preselect + rank: dyn edge bps m/slippage og buffer i ett kall
//    (Ingen vol-fallback her: rvbps må være finite og > 0.)
// [[Rcpp::export]]
List preselect_rank_cpp(
    IntegerVector mid_i,
    IntegerVector end_i,
    NumericVector edge,
    NumericMatrix BID,
    NumericMatrix ASK,
    NumericMatrix CAP,
    int start_i_1b,
    double fee_pct,
    double slip_base_frac,
    double buf_base_frac,
    double k_slip,
    NumericVector rvbps,
    int min_edge_bps,
    int topK
){
  int n = edge.size();
  if (mid_i.size()!=n || end_i.size()!=n || rvbps.size()!=n)
    stop("preselect_rank_cpp: length mismatch");
  
  LogicalVector keep = filter_candidates_cpp(mid_i, end_i, BID, ASK, CAP, start_i_1b);
  
  std::vector<int>    idx;  idx.reserve(n);
  std::vector<double> vals; vals.reserve(n);
  
  const double effF = eff_fee_3legs(fee_pct);
  
  for (int i=0; i<n; ++i){
    if (!keep[i]) continue;
    
    double e = edge[i];
    if (!std::isfinite(e)) continue;
    
    double v = rvbps[i];
    if (!std::isfinite(v) || v <= 0.0) continue;
    
    double slip_dyn = slip_base_frac + k_slip * (v/1e4);
    double thr = effF + 3.0*slip_dyn + buf_base_frac;
    double dyn_edge_bps = 1e4 * (e - thr);
    
    if (!std::isfinite(dyn_edge_bps)) continue;
    if (dyn_edge_bps < (double)min_edge_bps) continue;
    
    idx.push_back(i);
    vals.push_back(dyn_edge_bps);
  }
  
  int m = (int)idx.size();
  if (m==0) {
    return List::create(_["idx"] = IntegerVector(0),
                        _["dyn_edge_bps"] = NumericVector(0));
  }
  
  std::vector<int> order(m);
  for (int i=0;i<m;++i) order[i]=i;
  auto by_val_desc = [&](int a, int b){ return vals[a] > vals[b]; };
  
  if (topK>0 && topK<m) {
    std::partial_sort(order.begin(), order.begin()+topK, order.end(), by_val_desc);
    order.resize(topK);
  } else {
    std::sort(order.begin(), order.end(), by_val_desc);
  }
  
  int outN = (int)order.size();
  IntegerVector out_idx(outN);
  NumericVector out_vals(outN);
  for (int k=0;k<outN;++k){
    int p = order[k];
    out_idx[k] = idx[p] + 1;     // 1-basert posisjon i input
    out_vals[k]= vals[p];
  }
  
  return List::create(_["idx"] = out_idx,
                      _["dyn_edge_bps"] = out_vals);
}

// ------------------------------------------------------------------
// 3) Batch PnL for sanity-check før order
// [[Rcpp::export]]
DataFrame tri_pnl_batch_cpp(
    int start_i_1b,
    IntegerVector mid_i,
    IntegerVector end_i,
    NumericMatrix BID,
    NumericMatrix ASK,
    double start_qty_base,
    double fee_pct
){
  if (start_i_1b < 1) stop("start_i_1b must be >=1");
  int n = mid_i.size();
  if (end_i.size()!=n) stop("mid_i/end_i length mismatch");
  
  IntegerVector ok(n, 0), s1(n, NA_INTEGER), s2(n, NA_INTEGER), s3(n, NA_INTEGER);
  NumericVector final_base(n, NA_REAL), netPnL(n, NA_REAL), pnl_pct(n, NA_REAL);
  
  int s0 = start_i_1b - 1;
  
  auto convert_qty = [&](double qty, int from0, int to0, int &side)->double {
    double after = qty - fee_pct * qty;
    
    double rate_bid = BID(from0, to0);
    if (std::isfinite(rate_bid)) { side = 1; return after * rate_bid; }
    
    double ask_inv = ASK(to0, from0);
    if (std::isfinite(ask_inv) && ask_inv > 0.0) { side = 0; return after / ask_inv; }
    
    side = NA_INTEGER;
    return NA_REAL;
  };
  
  for (int k=0;k<n;++k){
    int m0 = mid_i[k]-1, e0 = end_i[k]-1;
    if (m0<0 || e0<0) continue;
    
    double amt = start_qty_base;
    int a1=NA_INTEGER, a2=NA_INTEGER, a3=NA_INTEGER;
    
    amt = convert_qty(amt, s0, m0, a1);
    if (!std::isfinite(amt)) { s1[k]=a1; continue; }
    
    amt = convert_qty(amt, m0, e0, a2);
    if (!std::isfinite(amt)) { s1[k]=a1; s2[k]=a2; continue; }
    
    amt = convert_qty(amt, e0, s0, a3);
    if (!std::isfinite(amt)) { s1[k]=a1; s2[k]=a2; s3[k]=a3; continue; }
    
    double fb = amt;
    double np = fb - start_qty_base;
    double pct = (start_qty_base>0.0) ? (100.0*np/start_qty_base) : NA_REAL;
    
    ok[k]=1; s1[k]=a1; s2[k]=a2; s3[k]=a3;
    final_base[k]=fb; netPnL[k]=np; pnl_pct[k]=pct;
  }
  
  return DataFrame::create(
    _["ok"]=ok,
    _["final_base"]=final_base,
    _["netPnL"]=netPnL,
    _["pnl_pct"]=pnl_pct,
    _["leg1_side"]=s1,
    _["leg2_side"]=s2,
    _["leg3_side"]=s3
  );
}

// ------------------------------------------------------------------
// 4) Vektorisert Kelly – ingen vol-fallback: ugyldig vol => min_units
// [[Rcpp::export]]
NumericVector kelly_size_vec_cpp(
    NumericVector edge_net_bps,
    NumericVector vol_bps_route,
    double bankroll,
    double frac,
    double cap,
    int    min_units,
    double bps_scale = 1e4,
    double var_floor = 1e-12
){
  int n = edge_net_bps.size();
  if (vol_bps_route.size()!=n) stop("kelly_size_vec_cpp: length mismatch");
  NumericVector out(n, NA_REAL);
  
  for (int i=0;i<n;++i){
    double mu_bps  = edge_net_bps[i];
    double vol_bps = vol_bps_route[i];
    
    if (!std::isfinite(mu_bps) || !std::isfinite(vol_bps) || vol_bps <= 0.0) {
      out[i] = std::max((double)min_units, 0.0);
      continue;
    }
    
    double mu  = mu_bps / bps_scale;
    double v   = vol_bps / bps_scale;
    double var = std::max(v*v, var_floor);
    double f   = std::max(0.0, std::min(1.0, mu/var)) * frac;
    double sz  = bankroll * f;
    
    if (!std::isfinite(sz)) sz = 0.0;
    if (sz < (double)min_units) sz = (double)min_units;
    if (sz > cap) sz = cap;
    out[i] = sz;
  }
  return out;
}
