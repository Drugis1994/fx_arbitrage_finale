// C.motor.cpp
// [[Rcpp::plugins(cpp11)]]
// macOS Big Sur / libc++ availability workaround
#if defined(__APPLE__) && !defined(_LIBCPP_DISABLE_AVAILABILITY)
#define _LIBCPP_DISABLE_AVAILABILITY
#endif
#ifdef Rcpp
#undef Rcpp
#endif
#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <limits>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <string>
using namespace Rcpp;

// ====== Små helpers ==========================================================
static inline bool finite_pos(double x){ return R_finite(x) && x > 0.0; }

// ============================================================================
//  A) UTILITETER (flyttet fra utils.R) — C++-akseleratorer
// ============================================================================

// -- A1) build_quote_map_cpp --------------------------------------------------
// Input: named list: name "BASE/QUOTE" -> list(bid, ask [,mid])
// Opsjoner: add_inverse (lag B/A ved behov), synth_usd (syntetiske kryss via USD)
// Return: named list: key -> list(bid, ask, mid)
// [[Rcpp::export]]
List build_quote_map_cpp(List latest_by_pair,
                         bool add_inverse = true,
                         bool synth_usd   = true) {
  CharacterVector keys = latest_by_pair.names();
  std::unordered_map<std::string, std::pair<double,double>> mp;
  mp.reserve(keys.size()*2);
  
  auto put = [&](const std::string& k, double bid, double ask){
    if (finite_pos(bid) && finite_pos(ask)) mp[k] = std::make_pair(bid, ask);
  };
  
  std::unordered_set<std::string> ccys;
  ccys.reserve(keys.size()*2);
  
  // 1) Les inn direkte quotes
  for (int i=0;i<keys.size();++i){
    std::string key = as<std::string>(keys[i]);
    size_t pos = key.find('/');
    if (pos == std::string::npos) continue;
    std::string a = key.substr(0,pos), b = key.substr(pos+1);
    ccys.insert(a); ccys.insert(b);
    
    List q = latest_by_pair[i];
    double bid = NA_REAL, ask = NA_REAL;
    if (q.containsElementNamed("bid")) bid = as<double>(q["bid"]);
    if (q.containsElementNamed("ask")) ask = as<double>(q["ask"]);
    if (finite_pos(bid) && finite_pos(ask)) put(key, bid, ask);
  }
  
  // 2) Lag inverse hvis mangler
  if (add_inverse){
    std::vector<std::pair<std::string,std::pair<double,double>>> to_add;
    to_add.reserve(mp.size());
    for (auto &kv : mp){
      const std::string &k = kv.first;
      size_t pos = k.find('/');
      if (pos == std::string::npos) continue;
      std::string a = k.substr(0,pos), b = k.substr(pos+1);
      std::string inv = b + "/" + a;
      if (mp.find(inv) == mp.end()){
        double bid = kv.second.first;
        double ask = kv.second.second;
        if (finite_pos(bid) && finite_pos(ask))
          to_add.push_back({inv, {1.0/ask, 1.0/bid}});
      }
    }
    for (auto &p: to_add) mp[p.first] = p.second;
  }
  
  // 3) Syntetiske via USD
  if (synth_usd && ccys.find("USD") != ccys.end()){
    auto fetch = [&](const std::string& k)->std::pair<bool,std::pair<double,double>>{
      auto it = mp.find(k);
      if (it==mp.end()) return {false,{NA_REAL,NA_REAL}};
      return {true,it->second};
    };
    std::vector<std::pair<std::string,std::pair<double,double>>> to_add;
    to_add.reserve(ccys.size()*ccys.size());
    
    for (const auto& a : ccys){
      for (const auto& b : ccys){
        if (a==b) continue;
        std::string k = a + "/" + b;
        if (mp.find(k) != mp.end()) continue;
        
        auto qa = fetch(a + "/USD");
        auto qb = fetch(b + "/USD");
        if (qa.first && qb.first){
          double bid = qa.second.first  / qb.second.second; // bid(a/usd)/ask(b/usd)
          double ask = qa.second.second / qb.second.first;  // ask(a/usd)/bid(b/usd)
          if (finite_pos(bid) && finite_pos(ask))
            to_add.push_back({k, {bid, ask}});
        }
      }
    }
    for (auto &p: to_add) mp[p.first] = p.second;
  }
  
  // 4) Bygg R-list
  // 4) Bygg R-list
  List out(mp.size());
  std::vector<std::string> onames;
  onames.reserve(mp.size());
  
  std::size_t idx_out = 0;
  for (const auto &kv : mp) {
    double bid = kv.second.first, ask = kv.second.second;
    double mid = 0.5 * (bid + ask);
    
    out[idx_out] = List::create(
      _["bid"] = bid,
      _["ask"] = ask,
      _["mid"] = mid
    );
    onames.push_back(kv.first);
    ++idx_out;
  }
  out.attr("names") = wrap(onames);
  return out;
}

// -- A2) make_price_matrices_cpp ----------------------------------------------
// qm: named list "BASE/QUOTE" -> list(bid,ask,mid)
// return: list(M,BID,ASK)
// [[Rcpp::export]]
List make_price_matrices_cpp(List qm) {
  CharacterVector keys = qm.names();
  int K = keys.size();
  
  // collect currencies
  std::unordered_map<std::string,int> idx;
  std::vector<std::string> ccys_vec;
  idx.reserve(K*2);
  ccys_vec.reserve(K*2);
  
  auto add_ccy = [&](const std::string &c){
    if (idx.find(c) == idx.end()) {
      int id = (int)idx.size();
      idx[c] = id;
      ccys_vec.push_back(c);
    }
  };
  
  for (int i=0;i<K;++i){
    std::string key = as<std::string>(keys[i]);
    size_t pos = key.find('/');
    if (pos == std::string::npos) continue;
    add_ccy(key.substr(0,pos));
    add_ccy(key.substr(pos+1));
  }
  
  int n = (int)idx.size();
  NumericMatrix M(n,n), BID(n,n), ASK(n,n);
  colnames(M)=wrap(ccys_vec); rownames(M)=wrap(ccys_vec);
  colnames(BID)=wrap(ccys_vec); rownames(BID)=wrap(ccys_vec);
  colnames(ASK)=wrap(ccys_vec); rownames(ASK)=wrap(ccys_vec);
  
  std::fill(M.begin(), M.end(), NA_REAL);
  std::fill(BID.begin(), BID.end(), NA_REAL);
  std::fill(ASK.begin(), ASK.end(), NA_REAL);
  for (int i=0;i<n;++i) M(i,i)=1.0;
  
  for (int i=0;i<K;++i){
    std::string key = as<std::string>(keys[i]);
    size_t pos = key.find('/');
    if (pos == std::string::npos) continue;
    std::string a = key.substr(0,pos);
    std::string b = key.substr(pos+1);
    
    int ia = idx[a], ib = idx[b];
    
    List q = qm[i];
    double bid = NA_REAL, ask = NA_REAL;
    if (q.containsElementNamed("bid")) bid = as<double>(q["bid"]);
    if (q.containsElementNamed("ask")) ask = as<double>(q["ask"]);
    if (finite_pos(bid)) BID(ia,ib) = bid;
    if (finite_pos(ask)) ASK(ia,ib) = ask;
    if (finite_pos(bid) && finite_pos(ask)) M(ia,ib) = 0.5*(bid+ask);
  }
  
  return List::create(_["M"]=M,_["BID"]=BID,_["ASK"]=ASK);
}

// -- A3) effektive trigger-formler --------------------------------------------
// [[Rcpp::export]] double eff_fee_3legs_cpp(double f){ return 1.0 - (1.0 - f)*(1.0 - f)*(1.0 - f); }
// [[Rcpp::export]]
double eff_fee_3legs_cpp(double f){ return 1.0 - (1.0 - f)*(1.0 - f)*(1.0 - f); }

// [[Rcpp::export]]
double edge_trigger_threshold_cpp(double fee_pct, double slip_frac, double buffer_frac){
  return eff_fee_3legs_cpp(fee_pct) + 3.0*slip_frac + buffer_frac;
}

// -- A4) side_used (for printing) ---------------------------------------------
// [[Rcpp::export]]
List side_used_cpp(int side, int from_i_1b, int to_i_1b,
                   CharacterVector ccys, NumericMatrix BID, NumericMatrix ASK){
  if (side==NA_INTEGER) return List::create(_["used"]="none", _["px"]=NA_REAL);
  int i = from_i_1b-1, j = to_i_1b-1;
  if (side==0){
    std::string used = std::string("ask ") + as<std::string>(ccys[to_i_1b-1]) + "/" + as<std::string>(ccys[from_i_1b-1]);
    return List::create(_["used"]=used, _["px"]=ASK(j,i));
  } else {
    std::string used = std::string("bid ") + as<std::string>(ccys[from_i_1b-1]) + "/" + as<std::string>(ccys[to_i_1b-1]);
    return List::create(_["used"]=used, _["px"]=BID(i,j));
  }
}

// -- A5) pair_spread_bps2_cpp -------------------------------------------------
// [[Rcpp::export]]
double pair_spread_bps2_cpp(std::string a, std::string b,
                            NumericMatrix BID, NumericMatrix ASK,
                            CharacterVector ccys){
  int n = ccys.size();
  std::unordered_map<std::string,int> map; map.reserve(n);
  for (int i=0;i<n;++i) map[ as<std::string>(ccys[i]) ] = i;
  
  auto ia = map.find(a), ib = map.find(b);
  if (ia==map.end() || ib==map.end()) return R_PosInf;
  double bid = BID(ia->second, ib->second), ask = ASK(ia->second, ib->second);
  if (!finite_pos(bid) || !finite_pos(ask)) return R_PosInf;
  double mid = 0.5*(bid+ask);
  if (!finite_pos(mid)) return R_PosInf;
  return (ask - bid) / mid * 1e4;
}

// -- A6) route_vol_bps_cpp ----------------------------------------------------
// [[Rcpp::export]]
double route_vol_bps_cpp(CharacterVector pairs,
                         NumericMatrix BID, NumericMatrix ASK,
                         CharacterVector ccys){
  int n = ccys.size();
  std::unordered_map<std::string,int> map; map.reserve(n);
  for (int i=0;i<n;++i) map[ as<std::string>(ccys[i]) ] = i;
  
  double sum_bps=0.0; int cnt=0;
  for (int k=0;k<pairs.size();++k){
    std::string p = as<std::string>(pairs[k]);
    size_t pos = p.find('/');
    if (pos==std::string::npos) continue;
    std::string a = p.substr(0,pos), b = p.substr(pos+1);
    
    auto ia = map.find(a), ib = map.find(b);
    if (ia==map.end() || ib==map.end()) continue;
    
    double bid = BID(ia->second, ib->second), ask = ASK(ia->second, ib->second);
    if (!finite_pos(bid) || !finite_pos(ask)) continue;
    double mid = 0.5*(bid+ask);
    if (!finite_pos(mid)) continue;
    
    double bps = (ask - bid) / mid * 1e4;
    sum_bps += bps; ++cnt;
  }
  if (cnt==0) return NA_REAL;
  return sum_bps / (double)cnt;
}

// ============================================================================
//  B) DIRTY-ROUTING ENGINE (som i koden din) + støttefunksjoner
// ============================================================================

struct Engine {
  // Konfig
  int n_ccy;
  int start0;                 // 0-basert start-indeks
  IntegerMatrix routes;       // [n_routes x 2], 1-basert i R
  
  // Matriser
  NumericMatrix M, BID, ASK;
  
  // CSR for ruter pr edge
  std::vector<int> edge_offsets; // size = n_edges + 1
  std::vector<int> edge_indices; // flat liste med rute-indekser (0-basert)
  
  // Dirty-set
  std::vector<char> dirty;
  std::vector<int>  dirty_list;
  
  // Preallokerte resultatbuffere
  std::vector<double> edge, dmid, vmid, mmid;
  std::vector<double> final_base, netPnL, pnl_pct;
  std::vector<int>    leg1_side, leg2_side, leg3_side;
  
  // Debounce
  double last_eval_ms = 0.0;
  
  // Ctor
  Engine(IntegerMatrix route_idx, int start_i_1b, int n_ccy_)
    : n_ccy(n_ccy_)
    , start0(start_i_1b - 1)
    , routes(route_idx)
    , M(), BID(), ASK()
    , edge_offsets(), edge_indices()
    , dirty(), dirty_list()
    , edge(), dmid(), vmid(), mmid()
    , final_base(), netPnL(), pnl_pct()
    , leg1_side(), leg2_side(), leg3_side()
  {
    const int n_routes = routes.nrow();
    dirty.assign(n_routes, 0);
    edge      .assign(n_routes, NA_REAL);
    dmid      .assign(n_routes, NA_REAL);
    vmid      .assign(n_routes, NA_REAL);
    mmid      .assign(n_routes, NA_REAL);
    final_base.assign(n_routes, NA_REAL);
    netPnL    .assign(n_routes, NA_REAL);
    pnl_pct   .assign(n_routes, NA_REAL);
    leg1_side .assign(n_routes, NA_INTEGER);
    leg2_side .assign(n_routes, NA_INTEGER);
    leg3_side .assign(n_routes, NA_INTEGER);
    edge_offsets.clear(); edge_indices.clear();
  }
  
  inline int edge_id(int i0, int j0) const { return i0 * n_ccy + j0; }
  
  void rebuild_participation() {
    const int n_edges = n_ccy * n_ccy;
    // 1) count per edge
    std::vector<int> counts(n_edges, 0);
    const int n_routes = routes.nrow();
    for (int k = 0; k < n_routes; ++k) {
      int mid0 = routes(k, 0) - 1;
      int end0 = routes(k, 1) - 1;
      if (mid0 < 0 || end0 < 0) continue;
      ++counts[ edge_id(start0, mid0) ];
      ++counts[ edge_id(mid0,  end0) ];
      ++counts[ edge_id(start0, end0) ];
    }
    
    // 2) prefix-sum -> start offsets (start[e] is where e's block begins)
    edge_offsets.assign(n_edges + 1, 0);
    edge_offsets[0] = 0;
    for (int e = 0; e < n_edges; ++e) {
      edge_offsets[e+1] = edge_offsets[e] + counts[e];
    }
    
    // 3) allocate index array and use 'cursor' initialized from starts
    edge_indices.assign(edge_offsets.back(), -1);
    std::vector<int> cursor(n_edges);
    for (int e = 0; e < n_edges; ++e) cursor[e] = edge_offsets[e];
    
    // 4) fill
    for (int k = 0; k < n_routes; ++k) {
      int mid0 = routes(k, 0) - 1;
      int end0 = routes(k, 1) - 1;
      if (mid0 < 0 || end0 < 0) continue;
      int e1 = edge_id(start0, mid0);
      int e2 = edge_id(mid0,  end0);
      int e3 = edge_id(start0, end0);
      edge_indices[ cursor[e1]++ ] = k;
      edge_indices[ cursor[e2]++ ] = k;
      edge_indices[ cursor[e3]++ ] = k;
    }
  }
  
  void set_matrices(NumericMatrix M_, NumericMatrix BID_, NumericMatrix ASK_) {
    M=M_; BID=BID_; ASK=ASK_;
  }
  
  void mark_cell(int i_1b, int j_1b) {
    int i0 = i_1b - 1, j0 = j_1b - 1;
    if (i0 < 0 || j0 < 0 || i0 >= n_ccy || j0 >= n_ccy) return;
    const int e = edge_id(i0, j0);
    if (edge_offsets.empty()) return;
    for (int p = edge_offsets[e]; p < edge_offsets[e + 1]; ++p) {
      int r = edge_indices[p];
      if (r >= 0 && !dirty[r]) { dirty[r] = 1; dirty_list.push_back(r); }
    }
  }
  void mark_pair(int i_1b, int j_1b) { mark_cell(i_1b, j_1b); mark_cell(j_1b, i_1b); }
  
  static inline double eff_fee_3legs(double f) { return 1.0 - (1.0 - f)*(1.0 - f)*(1.0 - f); }
  
  inline double route_edge_theoretical(int mid0, int end0) const {
    double s_to_m = M(start0, mid0);
    double m_to_e = M(mid0,  end0);
    double s_to_e = M(start0, end0);
    if (R_finite(s_to_m) && R_finite(m_to_e) && R_finite(s_to_e) && s_to_e > 0.0)
      return (s_to_m * m_to_e) / s_to_e - 1.0;
    return NA_REAL;
  }
  
  bool near_trigger(double fee_pct,
                    double slip_base_frac,
                    double buf_base_frac,
                    double k_slip,
                    double rvbps_fallback_bps,
                    double near_window_frac,
                    bool only_dirty) const {
    const double slip_dyn = slip_base_frac + k_slip * (rvbps_fallback_bps / 1e4);
    const double threshold = eff_fee_3legs(fee_pct) + 3.0 * slip_dyn + buf_base_frac;
    
    const int n_routes = routes.nrow();
    if (only_dirty) {
      for (int idx = 0; idx < (int)dirty_list.size(); ++idx) {
        int k = dirty_list[idx];
        int mid0 = routes(k, 0) - 1;
        int end0 = routes(k, 1) - 1;
        if (mid0 < 0 || end0 < 0) continue;
        double e = route_edge_theoretical(mid0, end0);
        if (R_finite(e) && e > (threshold - near_window_frac) && e < threshold) return true;
      }
      return false;
    } else {
      for (int k = 0; k < n_routes; ++k) {
        int mid0 = routes(k, 0) - 1;
        int end0 = routes(k, 1) - 1;
        if (mid0 < 0 || end0 < 0) continue;
        double e = route_edge_theoretical(mid0, end0);
        if (R_finite(e) && e > (threshold - near_window_frac) && e < threshold) return true;
      }
      return false;
    }
  }
  
  double max_edge_value() const {
    const int n_routes = routes.nrow();
    double best = R_NegInf;
    for (int k = 0; k < n_routes; ++k) {
      int mid0 = routes(k, 0) - 1;
      int end0 = routes(k, 1) - 1;
      if (mid0 < 0 || end0 < 0) continue;
      double e = route_edge_theoretical(mid0, end0);
      if (R_finite(e) && e > best) best = e;
    }
    return R_finite(best) ? best : NA_REAL;
  }
  
  void set_matrices_and_mark_changes(NumericMatrix M_, NumericMatrix BID_, NumericMatrix ASK_, double tol) {
    const bool have_old = (M.nrow() == n_ccy && M.ncol() == n_ccy);
    NumericMatrix Mold;
    if (have_old) Mold = clone(M);
    
    M = M_; BID = BID_; ASK = ASK_;
    
    if (!have_old || Mold.nrow() != n_ccy || Mold.ncol() != n_ccy) {
      const int n_routes = routes.nrow();
      for (int r = 0; r < n_routes; ++r) if (!dirty[r]) { dirty[r] = 1; dirty_list.push_back(r); }
      return;
    }
    for (int i = 0; i < n_ccy; ++i)
      for (int j = 0; j < n_ccy; ++j) {
        double a = Mold(i,j), b = M(i,j);
        bool changed =
          (R_IsNA(a) != R_IsNA(b)) ||
          (R_finite(a) && R_finite(b) && std::fabs(a - b) > tol) ||
          (!R_finite(a) && R_finite(b)) ||
          (R_finite(a) && !R_finite(b));
        if (changed) mark_cell(i+1, j+1);
      }
  }
  
  struct OutRow {
    int    k;
    int    mid0;
    int    end0;
    double edge;
    double final_base;
    double netPnL;
    double pnl_pct;
  };
  
  List eval_dirty(int K,
                  double fee_pct,
                  double /*slip_base_frac*/,
                  double /*buf_base_frac*/,
                  double /*k_slip*/,
                  double /*rvbps_fallback_bps*/,
                  double start_qty_base,
                  bool   profit_first) {
    std::vector<OutRow> rows;
    rows.reserve(dirty_list.size());
    
    for (int idx = 0; idx < (int)dirty_list.size(); ++idx) {
      int k = dirty_list[idx];
      dirty[k] = 0;
      
      int mid1 = routes(k, 0);
      int end1 = routes(k, 1);
      if (mid1 <= 0 || end1 <= 0) continue;
      int mid0 = mid1 - 1, end0 = end1 - 1;
      
      double s_to_m = M(start0, mid0);
      double m_to_e = M(mid0,  end0);
      double s_to_e = M(start0, end0);
      
      vmid[k] = s_to_m; mmid[k] = m_to_e; dmid[k] = s_to_e;
      
      double e = NA_REAL;
      if (R_finite(s_to_m) && R_finite(m_to_e) && R_finite(s_to_e) && s_to_e > 0.0)
        e = (s_to_m * m_to_e) / s_to_e - 1.0;
      edge[k] = e;
      
      double amt = start_qty_base;
      int s1=-1,s2=-1,s3=-1;
      
      // leg1
      {
        double ask_m_s = ASK(mid0, start0);
        double bid_s_m = BID(start0, mid0);
        double fee = fee_pct * amt;
        double after = amt - fee;
        if (R_finite(ask_m_s)) { amt = after / ask_m_s; s1 = 0; }
        else if (R_finite(bid_s_m)) { amt = after * bid_s_m; s1 = 1; }
        else { final_base[k]=netPnL[k]=pnl_pct[k]=NA_REAL; leg1_side[k]=NA_INTEGER; leg2_side[k]=NA_INTEGER; leg3_side[k]=NA_INTEGER; continue; }
      }
      // leg2
      {
        double ask_e_m = ASK(end0, mid0);
        double bid_m_e = BID(mid0, end0);
        double fee = fee_pct * amt;
        double after = amt - fee;
        if (R_finite(ask_e_m)) { amt = after / ask_e_m; s2 = 0; }
        else if (R_finite(bid_m_e)) { amt = after * bid_m_e; s2 = 1; }
        else { final_base[k]=netPnL[k]=pnl_pct[k]=NA_REAL; leg1_side[k]=s1; leg2_side[k]=NA_INTEGER; leg3_side[k]=NA_INTEGER; continue; }
      }
      // leg3
      {
        double ask_s_e = ASK(start0, end0);
        double bid_e_s = BID(end0, start0);
        double fee = fee_pct * amt;
        double after = amt - fee;
        if (R_finite(ask_s_e)) { amt = after / ask_s_e; s3 = 0; }
        else if (R_finite(bid_e_s)) { amt = after * bid_e_s; s3 = 1; }
        else { final_base[k]=netPnL[k]=pnl_pct[k]=NA_REAL; leg1_side[k]=s1; leg2_side[k]=s2; leg3_side[k]=NA_INTEGER; continue; }
      }
      
      final_base[k] = amt;
      netPnL[k]     = amt - start_qty_base;
      pnl_pct[k]    = (start_qty_base > 0.0) ? (100.0 * (amt - start_qty_base) / start_qty_base) : NA_REAL;
      leg1_side[k]  = s1; leg2_side[k] = s2; leg3_side[k] = s3;
      
      rows.push_back(OutRow{ k, mid0, end0, e, final_base[k], netPnL[k], pnl_pct[k] });
    }
    
    // --- Comparator: velg rangering basert på profit_first ---
    auto better_profit_first = [&](const OutRow& a, const OutRow& b){
      const bool ap = R_finite(a.netPnL) && a.netPnL > 0.0;
      const bool bp = R_finite(b.netPnL) && b.netPnL > 0.0;
      if (ap != bp) return ap;                 // positiv PnL vinner
      if (ap && bp) return a.netPnL > b.netPnL;
      const bool ae = R_finite(a.edge), be = R_finite(b.edge);
      if (ae != be) return ae;
      if (ae && be) return a.edge > b.edge;
      return a.k < b.k;
    };
    
    auto better_edge_first = [&](const OutRow& a, const OutRow& b){
      const bool ae = R_finite(a.edge), be = R_finite(b.edge);
      if (ae != be) return ae;
      if (ae && be) return a.edge > b.edge;
      const bool ap = R_finite(a.netPnL) && a.netPnL > 0.0;
      const bool bp = R_finite(b.netPnL) && b.netPnL > 0.0;
      if (ap != bp) return ap;
      if (ap && bp) return a.netPnL > b.netPnL;
      return a.k < b.k;
    };
    
    std::function<bool(const OutRow&, const OutRow&)> better;
    if (profit_first) better = better_profit_first;
    else              better = better_edge_first;
    
    auto worse = [&](const OutRow& a, const OutRow& b){ return !better(a,b); };
    
    // ------- Top-K utvalg -------
    std::vector<OutRow> topk;
    
    if (K <= 64 || K * 5 < (int)rows.size()) {
      topk.reserve(std::min(K, (int)rows.size()));
      for (const auto& r : rows) {
        if ((int)topk.size() < K) {
          topk.push_back(r);
          std::push_heap(topk.begin(), topk.end(), worse);
        } else if (better(r, topk.front())) {
          std::pop_heap(topk.begin(), topk.end(), worse);
          topk.back() = r;
          std::push_heap(topk.begin(), topk.end(), worse);
        }
      }
      std::sort_heap(topk.begin(), topk.end(), worse);
    } else {
      int outN = std::min(K, (int)rows.size());
      std::partial_sort(rows.begin(), rows.begin() + outN, rows.end(), better);
      topk.assign(rows.begin(), rows.begin() + outN);
    }
    
    int outN = (int)topk.size();
    
    IntegerVector mid_i(outN), end_i(outN), idx(outN);
    NumericVector v_edge(outN), v_final(outN), v_net(outN), v_pct(outN);
    IntegerVector v_s1(outN), v_s2(outN), v_s3(outN);
    NumericVector v_dmid(outN), v_vmid(outN), v_mmid(outN);
    
    for (int t = 0; t < outN; ++t) {
      const auto &r = topk[t];
      idx[t]    = r.k + 1;      // 1-basert
      mid_i[t]  = r.mid0 + 1;
      end_i[t]  = r.end0 + 1;
      v_edge[t] = r.edge;
      v_final[t]= r.final_base;
      v_net[t]  = r.netPnL;
      v_pct[t]  = r.pnl_pct;
      v_s1[t]   = leg1_side[r.k];
      v_s2[t]   = leg2_side[r.k];
      v_s3[t]   = leg3_side[r.k];
      v_dmid[t] = dmid[r.k];
      v_vmid[t] = vmid[r.k];
      v_mmid[t] = mmid[r.k];
    }
    // --- KOMPAKTER dirty_list: behold kun entries som fortsatt er dirty ---
    {
      std::vector<int> new_dirty;
      new_dirty.reserve(dirty_list.size());
      for (int id : dirty_list) {
        if (id >= 0 && id < (int)dirty.size() && dirty[id]) new_dirty.push_back(id);
      }
      dirty_list.swap(new_dirty);
    }
    
    return List::create(
      _["idx"]        = idx,
      _["mid_i"]      = mid_i,
      _["end_i"]      = end_i,
      _["edge"]       = v_edge,
      _["final_base"] = v_final,
      _["netPnL"]     = v_net,
      _["pnl_pct"]    = v_pct,
      _["leg1_side"]  = v_s1,
      _["leg2_side"]  = v_s2,
      _["leg3_side"]  = v_s3,
      _["dmid"]       = v_dmid,
      _["vmid"]       = v_vmid,
      _["mmid"]       = v_mmid
    );
  }
  
  List tick_once(int K,
                 double fee_pct,
                 double slip_base_frac,
                 double buf_base_frac,
                 double k_slip,
                 double rvbps_fallback_bps,
                 double start_qty_base,
                 double near_window_frac,
                 double now_ms,
                 int    min_interval_ms,
                 int    min_interval_fast_ms,
                 bool   profit_only) {
    bool near = near_trigger(fee_pct, slip_base_frac, buf_base_frac,
                             k_slip, rvbps_fallback_bps, near_window_frac, true);
    int gate_ms = near ? min_interval_fast_ms : min_interval_ms;
    if ((now_ms - last_eval_ms) < gate_ms) {
      return List::create(_["ran"]=false, _["near"]=near, _["result"]=R_NilValue);
    }
    last_eval_ms = now_ms;
    List res = eval_dirty(K, fee_pct, slip_base_frac, buf_base_frac,
                          k_slip, rvbps_fallback_bps, start_qty_base,
                          profit_only);
    return List::create(_["ran"]=true, _["near"]=near, _["result"]=res);
  }
}; // struct Engine

// ====== External ptr glue =====================================================
template <typename T>
void xp_finalizer(SEXP p) {
  if (TYPEOF(p) == EXTPTRSXP) {
    T* ptr = reinterpret_cast<T*>(R_ExternalPtrAddr(p));
    if (ptr) { delete ptr; R_ClearExternalPtr(p); }
  }
}
inline SEXP engine_wrap_ptr(Engine* e) {
  SEXP xp = R_MakeExternalPtr((void*)e, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(xp, xp_finalizer<Engine>, TRUE);
  return xp;
}
inline Engine* engine_from_xp(SEXP xp) {
  if (TYPEOF(xp) != EXTPTRSXP) stop("Invalid engine externalptr");
  Engine* e = reinterpret_cast<Engine*>(R_ExternalPtrAddr(xp));
  if (!e) stop("Null engine pointer");
  return e;
}

// ====== RCPP entrypoints ======================================================

// [[Rcpp::export]]
SEXP engine_create(IntegerMatrix route_idx, int start_i, int n_ccy) {
  if (start_i < 1 || start_i > n_ccy) stop("engine_create: start_i out of range");
  Engine* e = new Engine(route_idx, start_i, n_ccy);
  return engine_wrap_ptr(e);
}

// [[Rcpp::export]]
void engine_set_matrices(SEXP xp, NumericMatrix M, NumericMatrix BID, NumericMatrix ASK) {
  Engine* e = engine_from_xp(xp);
  if (M.nrow() != e->n_ccy || M.ncol() != e->n_ccy)  stop("M dims mismatch");
  if (BID.nrow() != e->n_ccy || BID.ncol() != e->n_ccy) stop("BID dims mismatch");
  if (ASK.nrow() != e->n_ccy || ASK.ncol() != e->n_ccy) stop("ASK dims mismatch");
  e->set_matrices(M, BID, ASK);
}

// [[Rcpp::export]]
void engine_set_matrices_and_mark_changes(SEXP xp,
                                          NumericMatrix M,
                                          NumericMatrix BID,
                                          NumericMatrix ASK,
                                          double tol = 1e-12) {
  Engine* e = engine_from_xp(xp);
  if (M.nrow() != e->n_ccy || M.ncol() != e->n_ccy)  stop("M dims mismatch");
  if (BID.nrow() != e->n_ccy || BID.ncol() != e->n_ccy) stop("BID dims mismatch");
  if (ASK.nrow() != e->n_ccy || ASK.ncol() != e->n_ccy) stop("ASK dims mismatch");
  e->set_matrices_and_mark_changes(M, BID, ASK, tol);
}

// [[Rcpp::export]]
void engine_rebuild_participation(SEXP xp) {
  engine_from_xp(xp)->rebuild_participation();
}

// [[Rcpp::export]]
void engine_mark_cell(SEXP xp, int i_1b, int j_1b) {
  engine_from_xp(xp)->mark_cell(i_1b, j_1b);
}

// [[Rcpp::export]]
void engine_mark_pair(SEXP xp, int i_1b, int j_1b) {
  engine_from_xp(xp)->mark_pair(i_1b, j_1b);
}

// [[Rcpp::export]]
List engine_eval_dirty(SEXP xp, int K,
                       double fee_pct,
                       double slip_base_frac,
                       double buf_base_frac,
                       double k_slip,
                       double rvbps_fallback_bps,
                       double start_qty_base,
                       bool   profit_only = false) {
  Engine* e = engine_from_xp(xp);
  return e->eval_dirty(K, fee_pct, slip_base_frac, buf_base_frac,
                       k_slip, rvbps_fallback_bps, start_qty_base,
                       profit_only);
}

// [[Rcpp::export]]
List engine_tick_once(SEXP xp,
                      int    K,
                      double fee_pct,
                      double slip_base_frac,
                      double buf_base_frac,
                      double k_slip,
                      double rvbps_fallback_bps,
                      double start_qty_base,
                      double near_window_frac,
                      double now_ms,
                      int    min_interval_ms,
                      int    min_interval_fast_ms,
                      bool   profit_only = false) {
  Engine* e = engine_from_xp(xp);
  return e->tick_once(K, fee_pct, slip_base_frac, buf_base_frac,
                      k_slip, rvbps_fallback_bps, start_qty_base,
                      near_window_frac, now_ms, min_interval_ms, min_interval_fast_ms,
                      profit_only);
}

// ===== Stand-alone helpers ====================================================

// [[Rcpp::export]]
DataFrame edge_screen_cpp(int s_i_1b, IntegerMatrix route_idx, NumericMatrix M) {
  if (s_i_1b < 1) stop("edge_screen_cpp: start index must be 1-based and >= 1");
  if (route_idx.ncol() < 2) stop("edge_screen_cpp: route_idx must have 2 columns");
  const int s0 = s_i_1b - 1;
  
  const int n = route_idx.nrow();
  IntegerVector mid_i(n), end_i(n);
  NumericVector dmid(n), vmid(n), mmid(n), edge(n);
  
  for (int k = 0; k < n; ++k) {
    int mid1 = route_idx(k, 0);
    int end1 = route_idx(k, 1);
    mid_i[k] = mid1;
    end_i[k] = end1;
    
    double d = NA_REAL, v = NA_REAL, m = NA_REAL, e = NA_REAL;
    if (mid1 > 0 && end1 > 0) {
      int mid0 = mid1 - 1;
      int end0 = end1 - 1;
      v = M(s0,  mid0);
      m = M(mid0, end0);
      d = M(s0,  end0);
      if (R_finite(v) && R_finite(m) && R_finite(d) && d > 0.0)
        e = (v * m) / d - 1.0;
    }
    dmid[k] = d; vmid[k] = v; mmid[k] = m; edge[k] = e;
  }
  
  return DataFrame::create(
    _["mid_i"] = mid_i,
    _["end_i"] = end_i,
    _["dmid"]  = dmid,
    _["vmid"]  = vmid,
    _["mmid"]  = mmid,
    _["edge"]  = edge,
    _["stringsAsFactors"] = false
  );
}

// [[Rcpp::export]]
DataFrame tri_pnl_cpp(int start_i_1b,
                      IntegerMatrix route_idx,
                      NumericMatrix BID,
                      NumericMatrix ASK,
                      double start_qty_base,
                      double fee_pct) {
  if (start_i_1b < 1) stop("tri_pnl_cpp: start index must be 1-based and >= 1");
  if (route_idx.nrow() < 1 || route_idx.ncol() < 2)
    stop("tri_pnl_cpp: route_idx must be at least one row and 2 columns");
  
  const int s0 = start_i_1b - 1;
  const int mid0 = route_idx(0, 0) - 1;
  const int end0 = route_idx(0, 1) - 1;
  
  auto convert_qty = [&](double qty, int from0, int to0, int &side)->double {
    double after = qty - fee_pct * qty;
    double rate_bid = BID(from0, to0);
    if (R_finite(rate_bid)) { side = 1; return after * rate_bid; }
    double ask_inv = ASK(to0, from0);
    if (R_finite(ask_inv) && ask_inv > 0.0) { side = 0; return after / ask_inv; }
    side = NA_INTEGER;
    return NA_REAL;
  };
  
  double amt = start_qty_base;
  int s1 = NA_INTEGER, s2 = NA_INTEGER, s3 = NA_INTEGER;
  
  amt = convert_qty(amt, s0,  mid0, s1);
  if (!R_finite(amt)) {
    return DataFrame::create(_["ok"]=false,
                             _["final_base"]=NA_REAL, _["netPnL"]=NA_REAL, _["pnl_pct"]=NA_REAL,
                             _["leg1_side"]=NA_INTEGER, _["leg2_side"]=NA_INTEGER, _["leg3_side"]=NA_INTEGER,
                               _["stringsAsFactors"]=false);
  }
  amt = convert_qty(amt, mid0, end0, s2);
  if (!R_finite(amt)) {
    return DataFrame::create(_["ok"]=false,
                             _["final_base"]=NA_REAL, _["netPnL"]=NA_REAL, _["pnl_pct"]=NA_REAL,
                             _["leg1_side"]=s1, _["leg2_side"]=NA_INTEGER, _["leg3_side"]=NA_INTEGER,
                               _["stringsAsFactors"]=false);
  }
  amt = convert_qty(amt, end0, s0, s3);
  if (!R_finite(amt)) {
    return DataFrame::create(_["ok"]=false,
                             _["final_base"]=NA_REAL, _["netPnL"]=NA_REAL, _["pnl_pct"]=NA_REAL,
                             _["leg1_side"]=s1, _["leg2_side"]=s2, _["leg3_side"]=NA_INTEGER,
                             _["stringsAsFactors"]=false);
  }
  
  double final_base = amt;
  double netPnL     = final_base - start_qty_base;
  double pnl_pct    = (start_qty_base > 0.0) ? (100.0 * netPnL / start_qty_base) : NA_REAL;
  
  return DataFrame::create(
    _["ok"]         = true,
    _["final_base"] = final_base,
    _["netPnL"]     = netPnL,
    _["pnl_pct"]    = pnl_pct,
    _["leg1_side"]  = s1,
    _["leg2_side"]  = s2,
    _["leg3_side"]  = s3,
    _["stringsAsFactors"] = false
  );
}







// ---------- order_depth_partial_cpp -------------------------------------------------
// Partial-fill aware orderbook depth check implemented in C++ for speed.
// Place this function in C.motor.cpp among the stand-alone helpers (after tri_pnl_cpp).
//
// Inputs:
//  - price: NumericVector of prices per level
//  - units: NumericVector of units (base currency) per level
//  - sideVec: CharacterVector with side indicator per level (optional, length==0 => no side filtering)
//  - side: "buy" => we will consume ASK levels (best low price first). "sell" => consume BID levels (best high price first).
//  - need_quote: If finite => attempt to consume until need_quote * (1 + safety_buffer_bps/10000) is satisfied.
//                If NA, we just return cum sums up to depth_levels.
//  - depth_levels: how many levels at most to consider
//  - safety_buffer_bps: safety buffer in bps (2 => 0.0002 fraction)
//
// Output: R list { ok, cum_quote, cum_base, avg_price, checked_levels, filled (bool if need_quote satisfied),
//                  filled_level_index (1-based into selected indices, NA if not applicable), partial_base_from_level,
//                  selected_indices (0-based indices of levels considered) }
// [[Rcpp::export]]
Rcpp::List order_depth_partial_cpp(Rcpp::NumericVector price,
                                   Rcpp::NumericVector units,
                                   Rcpp::CharacterVector sideVec, // may be length 0
                                   std::string side,               // "buy" or "sell"
                                   double need_quote,              // NA_REAL if not provided
                                   int depth_levels,
                                   double safety_buffer_bps) {
  using namespace Rcpp;
  int n = price.size();
  if (n != (int)units.size()) stop("price and units must have same length");
  
  struct Row { double p; double u; std::string s; int idx; };
  std::vector<Row> rows;
  rows.reserve(n);
  bool haveSideVec = (sideVec.size() == n);
  
  for (int i = 0; i < n; ++i) {
    double pi = price[i];
    double ui = units[i];
    if (!R_finite(pi) || !R_finite(ui)) continue;
    if (ui <= 0.0) continue;                // SKIP zero/negative units (NY)
    std::string sv = haveSideVec ? as<std::string>(sideVec[i]) : std::string();
    for (auto &c : sv) c = static_cast<char>(toupper((unsigned char)c));
    rows.push_back(Row{pi, ui, sv, i});
  }
  
  bool wantBuy = (side == "buy" || side == "BUY" || side == "Buy");
  if (haveSideVec) {
    std::vector<Row> tmp; tmp.reserve(rows.size());
    for (auto &r : rows) {
      if (wantBuy) {
        if (r.s == "ASK" || r.s == "SELL" || r.s == "OFFER") tmp.push_back(r);
      } else {
        if (r.s == "BID" || r.s == "BUY") tmp.push_back(r);
      }
    }
    rows.swap(tmp);
  }
  
  if (rows.empty()) {
    return List::create(_["ok"]=false,
                        _["cum_quote"]=0.0,
                        _["cum_base"]=0.0,
                        _["avg_price"]=NA_REAL,
                        _["checked_levels"]=0,
                        _["partial_base_from_level"]=NA_REAL,
                        _["filled_level_pos"]=NA_INTEGER,
                        _["filled_level_global_idx"]=NA_INTEGER, // NYTT
                        _["filled"]=false);
  }
  
  if (wantBuy) {
    std::sort(rows.begin(), rows.end(), [](const Row &a, const Row &b){ return a.p < b.p; });
  } else {
    std::sort(rows.begin(), rows.end(), [](const Row &a, const Row &b){ return a.p > b.p; });
  }
  
  int levels_to_check = std::min(depth_levels, (int)rows.size());
  double s_buffer = 1.0 + safety_buffer_bps / 10000.0;
  
  double cum_quote = 0.0;
  double cum_base  = 0.0;
  double partial_base_from_level = NA_REAL;
  int filled_level_pos = NA_INTEGER;
  int filled_level_global_idx = NA_INTEGER; // NYTT
  bool filled = false;
  
  bool need_given = R_finite(need_quote);
  int actual_checked = 0; // teller gyldige nivåer vi faktisk vurderer
  
  for (int i = 0; i < levels_to_check; ++i) {
    const Row &r = rows[i];
    double price_i = r.p;
    double units_i = r.u;
    if (!R_finite(price_i) || !R_finite(units_i) || units_i <= 0.0) continue; // ekstra guard
    double val_i_quote = price_i * units_i;
    ++actual_checked;
    
    if (need_given) {
      double target = need_quote * s_buffer;
      if (cum_quote + val_i_quote >= target) {
        double remaining_quote = target - cum_quote;
        if (remaining_quote < 0.0) remaining_quote = 0.0;
        if (price_i > 0.0) {
          double base_from_level = remaining_quote / price_i;
          cum_base += base_from_level;
          cum_quote += remaining_quote;
          partial_base_from_level = base_from_level;
          filled_level_pos = i + 1; // 1-based pos in sorted array
          filled_level_global_idx = r.idx + 1; // 1-based original index (NYTT)
          filled = true;
        }
        break;
      } else {
        cum_base += units_i;
        cum_quote += val_i_quote;
      }
    } else {
      cum_base += units_i;
      cum_quote += val_i_quote;
    }
  }
  
  if (need_given && !filled) {
    filled = (cum_quote >= (need_quote * s_buffer));
    if (!filled) {
      partial_base_from_level = NA_REAL;
      filled_level_pos = NA_INTEGER;
      filled_level_global_idx = NA_INTEGER;
    }
  }
  
  double avg_price = NA_REAL;
  if (cum_base > 0.0) avg_price = cum_quote / cum_base;
  
  return List::create(
    _["ok"] = (need_given ? filled : true),
    _["cum_quote"] = cum_quote,
    _["cum_base"] = cum_base,
    _["avg_price"] = avg_price,
    _["checked_levels"] = actual_checked,             // endret: faktisk sjekket
    _["partial_base_from_level"] = partial_base_from_level,
    _["filled_level_pos"] = filled_level_pos,
    _["filled_level_global_idx"] = filled_level_global_idx, // nytt felt
    _["filled"] = filled
  );
}








// ---- New: order_depth_split_cpp (split-level v2) ----
// [[Rcpp::export]]
Rcpp::List order_depth_split_cpp(Rcpp::NumericVector price,
                                 Rcpp::NumericVector units,
                                 Rcpp::CharacterVector sideVec, // may be length 0
                                 std::string side,               // "buy" or "sell"
                                 double need_quote,              // NA_REAL if not provided
                                 int depth_levels,
                                 double safety_buffer_bps) {
  using namespace Rcpp;
  int n = price.size();
  if (n != (int)units.size()) stop("price and units must have same length");
  struct Row { double p; double u; std::string s; int idx; };
  std::vector<Row> rows; rows.reserve(n);
  bool haveSideVec = (sideVec.size() == n);
  
  for (int i = 0; i < n; ++i) {
    double pi = price[i];
    double ui = units[i];
    if (!R_finite(pi) || !R_finite(ui) || ui <= 0.0) continue;
    std::string sv = haveSideVec ? as<std::string>(sideVec[i]) : std::string();
    for (auto &c : sv) c = static_cast<char>(toupper((unsigned char)c));
    rows.push_back(Row{pi, ui, sv, i});
  }
  
  bool wantBuy = (side == "buy" || side == "BUY" || side == "Buy");
  if (haveSideVec) {
    std::vector<Row> tmp; tmp.reserve(rows.size());
    for (auto &r : rows) {
      if (wantBuy) {
        if (r.s == "ASK" || r.s == "SELL" || r.s == "OFFER") tmp.push_back(r);
      } else {
        if (r.s == "BID" || r.s == "BUY") tmp.push_back(r);
      }
    }
    rows.swap(tmp);
  }
  
  if (rows.empty()) {
    return List::create(_["ok"]=false,
                        _["cum_quote"]=0.0,
                        _["cum_base"]=0.0,
                        _["avg_price"]=NumericVector::get_na(),
                        _["checked_levels"]=0,
                        _["partial_base_from_level"]=NumericVector::get_na(),
                        _["filled_level_pos"]=IntegerVector::create(NA_INTEGER),
                        _["filled_level_global_idx"]=IntegerVector::create(NA_INTEGER),
                        _["filled"]=false,
                        _["used_units"]=NumericVector::create(), 
                        _["used_quote"]=NumericVector::create(), 
                        _["level_filled"]=LogicalVector::create());
  }
  
  if (wantBuy) {
    std::sort(rows.begin(), rows.end(), [](const Row &a, const Row &b){ return a.p < b.p; });
  } else {
    std::sort(rows.begin(), rows.end(), [](const Row &a, const Row &b){ return a.p > b.p; });
  }
  
  int levels_to_check = std::min(depth_levels, (int)rows.size());
  double s_buffer = 1.0 + safety_buffer_bps / 10000.0;
  bool need_given = R_finite(need_quote);
  
  double cum_quote = 0.0;
  double cum_base = 0.0;
  double partial_base_from_level = NumericVector::get_na();
  int filled_level_pos = NA_INTEGER;
  int filled_level_global_idx = NA_INTEGER;
  bool filled = false;
  
  NumericVector used_units(levels_to_check);
  NumericVector used_quote(levels_to_check);
  LogicalVector level_filled(levels_to_check);
  
  for (int i = 0; i < levels_to_check; ++i) {
    const Row &r = rows[i];
    double price_i = r.p;
    double units_i = r.u;
    double val_i_quote = price_i * units_i;
    used_units[i] = 0.0;
    used_quote[i] = 0.0;
    level_filled[i] = false;
    
    if (need_given) {
      double target = need_quote * s_buffer;
      if (cum_quote + val_i_quote >= target) {
        double remaining_quote = target - cum_quote;
        if (remaining_quote <= 0.0) {
          // already satisfied (shouldn't normally happen)
        } else {
          double base_from_level = remaining_quote / price_i;
          // enforce per-level cap
          if (base_from_level > units_i) base_from_level = units_i;
          double used_q = base_from_level * price_i;
          cum_base += base_from_level;
          cum_quote += used_q;
          used_units[i] = base_from_level;
          used_quote[i] = used_q;
          partial_base_from_level = base_from_level;
          filled_level_pos = i + 1;
          filled_level_global_idx = r.idx + 1; // 1-based global index
          filled = (cum_quote >= target - 1e-12);
          level_filled[i] = (base_from_level >= units_i - 1e-12);
        }
        break;
      } else {
        // consume entire level
        cum_base += units_i;
        cum_quote += val_i_quote;
        used_units[i] = units_i;
        used_quote[i] = val_i_quote;
        level_filled[i] = true;
      }
    } else {
      // no target: just sum up to depth_levels
      cum_base += units_i;
      cum_quote += val_i_quote;
      used_units[i] = units_i;
      used_quote[i] = val_i_quote;
      level_filled[i] = true;
    }
  }
  
  if (need_given && !filled) {
    // final check if exact equality reached
    filled = (cum_quote >= (need_quote * s_buffer));
    if (!filled) {
      partial_base_from_level = NumericVector::get_na();
      filled_level_pos = NA_INTEGER;
      filled_level_global_idx = NA_INTEGER;
    }
  }
  
  double avg_price = NumericVector::get_na();
  if (cum_base > 0.0) avg_price = cum_quote / cum_base;
  
  return List::create(
    _["ok"] = (need_given ? filled : true),
    _["cum_quote"] = cum_quote,
    _["cum_base"] = cum_base,
    _["avg_price"] = avg_price,
    _["checked_levels"] = levels_to_check,
    _["partial_base_from_level"] = partial_base_from_level,
    _["filled_level_pos"] = filled_level_pos,
    _["filled_level_global_idx"] = filled_level_global_idx,
    _["filled"] = filled,
    _["used_units"] = used_units,
    _["used_quote"] = used_quote,
    _["level_filled"] = level_filled
  );
}










// [[Rcpp::export]]
bool engine_near_trigger(SEXP xp,
                         double fee_pct,
                         double slip_base_frac,
                         double buf_base_frac,
                         double k_slip,
                         double rvbps_fallback_bps,
                         double near_window_frac,
                         bool only_dirty = true) {
  Engine* e = engine_from_xp(xp);
  return e->near_trigger(fee_pct, slip_base_frac, buf_base_frac, k_slip,
                         rvbps_fallback_bps, near_window_frac, only_dirty);
}

// [[Rcpp::export]]
double engine_max_edge(SEXP xp) {
  return engine_from_xp(xp)->max_edge_value();
}
