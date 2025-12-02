// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <limits>
#include <cmath>

using namespace Rcpp;



// --- Helper: best-of-two conversion ---
// Returnerer ny qty (etter gebyr). Setter side (1=BID, 0=inverse ASK) og c_rate (effektiv faktor).
static inline double convert_best(double qty_after_fee,
                                  int from0, int to0,
                                  const NumericMatrix& BID,
                                  const NumericMatrix& ASK,
                                  int& side, double& c_rate)
{
  side   = NA_INTEGER;
  c_rate = NA_REAL;
  
  const double r_bid    = BID(from0, to0);
  const double r_askInv = ASK(to0,  from0); // må være > 0 for å kunne inverteres
  
  const bool have_bid    = R_finite(r_bid);
  const bool have_invAsk = R_finite(r_askInv) && r_askInv > 0.0;
  
  const double cand_bid    = have_bid    ? qty_after_fee * r_bid     : NA_REAL;
  const double cand_invAsk = have_invAsk ? qty_after_fee / r_askInv  : NA_REAL;
  
  if (R_finite(cand_bid) && (!R_finite(cand_invAsk) || cand_bid >= cand_invAsk)) {
    side   = 1;
    c_rate = r_bid;            // effektiv faktor ved BID
    return cand_bid;
  } else if (R_finite(cand_invAsk)) {
    side   = 0;
    c_rate = 1.0 / r_askInv;   // effektiv faktor ved inverse ASK
    return cand_invAsk;
  } else {
    return NA_REAL;
  }
}




// ==============================
  //  DIRTY-ROUTING ENGINE (REN)
// ==============================
  struct Engine {
    // Konfig
    int n_ccy;
    int start0;                 // 0-basert start-indeks
    IntegerMatrix routes;       // [n_routes x 2], 1-basert i R
    
    // Matriser
    NumericMatrix M, BID, ASK;
    
    // --- C4) CSR for ruter som deltar i hver M-edge ---
  // I stedet for vector<vector<int>> routes_by_edge;
std::vector<int> edge_offsets; // size = n_edges + 1, prefix sums
std::vector<int> edge_indices; // flat liste med rute-indekser (0-basert)

// Dirty-set
std::vector<char> dirty;       // flag per rute
std::vector<int>  dirty_list;  // kompakt liste (0-basert)

// Preallokerte resultatbuffere
std::vector<double> edge, dmid, vmid, mmid;
std::vector<double> final_base, netPnL, pnl_pct;
std::vector<int>    leg1_side, leg2_side, leg3_side;

// Debounce / timing
double last_eval_ms = 0.0;     // epoch ms forrige evaluering

// -------- FIXED CONSTRUCTOR (body added) --------
  Engine(IntegerMatrix route_idx, int start_i_1b, int n_ccy_)
: n_ccy(n_ccy_),
start0(start_i_1b - 1),
routes(route_idx),
M(), BID(), ASK(),
edge_offsets(), edge_indices(),
dirty(), dirty_list(),
edge(), dmid(), vmid(), mmid(),
final_base(), netPnL(), pnl_pct(),
leg1_side(), leg2_side(), leg3_side()
{
  const int n_routes = routes.nrow();
  
  // dirty + result-buffere
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
  
  // CSR-arrays init tomme; fylles i rebuild_participation()
  edge_offsets.clear();
  edge_indices.clear();
}

// -------- Helpers / core methods (inside struct) --------
  inline int edge_id(int i0, int j0) const { return i0 * n_ccy + j0; } // 0-basert

// --- C4) bygg CSR: hvilke ruter hører til hver matrise-edge (i,j) ---
  void rebuild_participation() {
    const int n_edges = n_ccy * n_ccy;
    edge_offsets.assign(n_edges + 1, 0);
    
    // 1) Count forekomster per edge
const int n_routes = routes.nrow();
for (int k = 0; k < n_routes; ++k) {
  int mid0 = routes(k, 0) - 1;
  int end0 = routes(k, 1) - 1;
  if (mid0 < 0 || end0 < 0) continue;
  
  // Hver rute avhenger av M[start,mid], M[mid,end], M[start,end]
  ++edge_offsets[ edge_id(start0, mid0) ];
  ++edge_offsets[ edge_id(mid0,  end0) ];
  ++edge_offsets[ edge_id(start0, end0) ];
}

// 2) Prefix sum -> offsets
for (int e = 1; e <= n_edges; ++e)
  edge_offsets[e] += edge_offsets[e - 1];
  
  // 3) Alloker flat indeksliste
edge_indices.assign(edge_offsets.back(), -1);

// 4) Fyll ved å bruke en cursor-kopi av offsets
std::vector<int> cursor = edge_offsets;
for (int k = 0; k < n_routes; ++k) {
  int mid0 = routes(k, 0) - 1;
  int end0 = routes(k, 1) - 1;
  if (mid0 < 0 || end0 < 0) continue;
  
  edge_indices[ cursor[ edge_id(start0, mid0) ]++ ] = k;
  edge_indices[ cursor[ edge_id(mid0,  end0) ]++ ] = k;
  edge_indices[ cursor[ edge_id(start0, end0) ]++ ] = k;
}
  }

void set_matrices(NumericMatrix M_, NumericMatrix BID_, NumericMatrix ASK_) {
  M = M_; BID = BID_; ASK = ASK_;
}

// --- C4) mark_cell via CSR ---
  void mark_cell(int i_1b, int j_1b) {
    int i0 = i_1b - 1;
    int j0 = j_1b - 1;
    if (i0 < 0 || j0 < 0 || i0 >= n_ccy || j0 >= n_ccy) return;
    
    const int e = edge_id(i0, j0);
    if (edge_offsets.empty()) return; // ikke bygget ennå
    
    for (int p = edge_offsets[e]; p < edge_offsets[e + 1]; ++p) {
      int r = edge_indices[p];
      if (r >= 0 && !dirty[r]) { dirty[r] = 1; dirty_list.push_back(r); }
    }
  }

void mark_pair(int i_1b, int j_1b) { mark_cell(i_1b, j_1b); mark_cell(j_1b, i_1b); }

static inline double eff_fee_3legs(double f) {
  // 1 - (1 - f)^3 — robust for små f
  return 1.0 - (1.0 - f) * (1.0 - f) * (1.0 - f);
}

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
                  bool only_dirty) const
{
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

// ---------- Matriseoppdatering + diff -> mark dirty ----------
  void set_matrices_and_mark_changes(NumericMatrix M_, NumericMatrix BID_, NumericMatrix ASK_, double tol) {
    const bool have_old = (M.nrow() == n_ccy && M.ncol() == n_ccy);
    NumericMatrix Mold;
    if (have_old) Mold = clone(M);    // deep copy for diff
    
    M = M_; BID = BID_; ASK = ASK_;
    
    if (!have_old || Mold.nrow() != n_ccy || Mold.ncol() != n_ccy) {
      // Ingen tidligere M: marker alle ruter
      const int n_routes = routes.nrow();
      for (int r = 0; r < n_routes; ++r) if (!dirty[r]) { dirty[r] = 1; dirty_list.push_back(r); }
      return;
    }
    
    // Diff Mold vs M: NA/finite endringer eller |diff| > tol
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
  int    k;        // rute-indeks (0-basert)
  int    mid0;     // 0-basert
  int    end0;     // 0-basert
  double edge;
  double final_base;
  double netPnL;
  double pnl_pct;
};

// ---------- Evaluer top-K blant dirty ruter ----------
List eval_dirty(int K,
                double fee_pct,
                double /*slip_base_frac*/,
                double /*buf_base_frac*/,
                double /*k_slip*/,
                double /*rvbps_fallback_bps*/,
                double start_qty_base)
{
  std::vector<OutRow> rows;
  rows.reserve(dirty_list.size());
  
  for (int idx = 0; idx < (int)dirty_list.size(); ++idx) {
    int k = dirty_list[idx];
    dirty[k] = 0; // reset flag
    
    int mid1 = routes(k, 0); // 1-basert
    int end1 = routes(k, 1);
    if (mid1 <= 0 || end1 <= 0) continue;
    int mid0 = mid1 - 1;
    int end0 = end1 - 1;
    
    // hent M-komponenter
    double s_to_m = M(start0, mid0);
    double m_to_e = M(mid0,  end0);
    double s_to_e = M(start0, end0);
    
    vmid[k] = s_to_m;
    mmid[k] = m_to_e;
    dmid[k] = s_to_e;
    
    double e = NA_REAL;
    if (R_finite(s_to_m) && R_finite(m_to_e) && R_finite(s_to_e) && s_to_e > 0.0) {
      e = (s_to_m * m_to_e) / s_to_e - 1.0;
    }
    edge[k] = e;
    
    // PnL sim (3 steg, gebyr per steg) — BEST-OF-TWO hver legg
    double amt = start_qty_base;
    int s1 = NA_INTEGER, s2 = NA_INTEGER, s3 = NA_INTEGER;
    
    // step1: start -> mid
    {
      double after = amt - fee_pct * amt;
      double c_rate;
      amt = convert_best(after, start0, mid0, BID, ASK, s1, c_rate);
      if (!R_finite(amt)) {
        final_base[k]=netPnL[k]=pnl_pct[k]=NA_REAL;
        leg1_side[k]=leg2_side[k]=leg3_side[k]=NA_INTEGER;
        continue;
      }
    }
    
    // step2: mid -> end
    {
      double after = amt - fee_pct * amt;
      double c_rate;
      amt = convert_best(after, mid0, end0, BID, ASK, s2, c_rate);
      if (!R_finite(amt)) {
        final_base[k]=netPnL[k]=pnl_pct[k]=NA_REAL;
        leg1_side[k]=s1; leg2_side[k]=leg3_side[k]=NA_INTEGER;
        continue;
      }
    }
    
    // step3: end -> start
    {
      double after = amt - fee_pct * amt;
      double c_rate;
      amt = convert_best(after, end0, start0, BID, ASK, s3, c_rate);
      if (!R_finite(amt)) {
        final_base[k]=netPnL[k]=pnl_pct[k]=NA_REAL;
        leg1_side[k]=s1; leg2_side[k]=s2; leg3_side[k]=NA_INTEGER;
        continue;
      }
    }
    
    final_base[k] = amt;
    netPnL[k]     = amt - start_qty_base;
    pnl_pct[k]    = (start_qty_base > 0.0) ? (100.0 * (amt - start_qty_base) / start_qty_base) : NA_REAL;
    leg1_side[k]  = s1;
    leg2_side[k]  = s2;
    leg3_side[k]  = s3;
    
    rows.push_back(OutRow{ k, mid0, end0, e, final_base[k], netPnL[k], pnl_pct[k] });
  }
  
  // Tøm dirty-listen for neste runde
  dirty_list.clear();
  
  // --- Adaptive Top-K (heap for små K, partial_sort for store) ---
  auto better = [&](const OutRow& a, const OutRow& b){
    const bool ap = R_finite(a.netPnL) && a.netPnL > 0.0;
    const bool bp = R_finite(b.netPnL) && b.netPnL > 0.0;
    if (ap != bp) return ap; // true = bedre
    if (ap && bp) return a.netPnL > b.netPnL;
    const bool ae = R_finite(a.edge), be = R_finite(b.edge);
    if (ae != be) return ae;
    if (ae && be) return a.edge > b.edge;
    return a.k < b.k;
  };
  auto worse = [&](const OutRow& a, const OutRow& b){ return !better(a,b); };
  
  std::vector<OutRow> topK;
  if (K > 0) {
    if (K <= 64 || K * 5 < (int)rows.size()) {
      topK.reserve(std::min(K, (int)rows.size()));
      for (const auto& r : rows) {
        if ((int)topK.size() < K) {
          topK.push_back(r);
          std::push_heap(topK.begin(), topK.end(), worse);
        } else if (better(r, topK.front())) {
          std::pop_heap(topK.begin(), topK.end(), worse);
          topK.back() = r;
          std::push_heap(topK.begin(), topK.end(), worse);
        }
      }
      std::sort_heap(topK.begin(), topK.end(), worse);
    } else {
      int outN = std::min<int>(K, (int)rows.size());
      std::partial_sort(rows.begin(), rows.begin()+outN, rows.end(), better);
      topK.assign(rows.begin(), rows.begin()+outN);
    }
  }
  
  int outN = (int)topK.size();
  IntegerVector mid_i(outN), end_i(outN), idx(outN);
  NumericVector v_edge(outN), v_final(outN), v_net(outN), v_pct(outN);
  IntegerVector v_s1(outN), v_s2(outN), v_s3(outN);
  NumericVector v_dmid(outN), v_vmid(outN), v_mmid(outN);
  
  for (int t = 0; t < outN; ++t) {
    const auto &r = topK[t];        // <-- riktig: bruk topK, ikke rows
    idx[t]    = r.k + 1;            // 1-basert rute-rad for R
    mid_i[t]  = r.mid0 + 1;         // 1-basert
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
// ---------- One-shot tick: debounce + near-trigger + evaluate ----------
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
                 int    min_interval_fast_ms)
{
  // Decide near-trigger based on dirty routes only (cheap)
  bool near = near_trigger(fee_pct, slip_base_frac, buf_base_frac,
                           k_slip, rvbps_fallback_bps, near_window_frac, true);
  
  int gate_ms = near ? min_interval_fast_ms : min_interval_ms;
  
  if ((now_ms - last_eval_ms) < gate_ms) {
    return List::create(
      _["ran"]    = false,
      _["near"]   = near,
      _["result"] = R_NilValue
    );
  }
  
  last_eval_ms = now_ms;
  List res = eval_dirty(K, fee_pct, slip_base_frac, buf_base_frac,
                        k_slip, rvbps_fallback_bps, start_qty_base);
  
  return List::create(
    _["ran"]    = true,
    _["near"]   = near,
    _["result"] = res
  );
  }
  }; // end struct Engine

// ---------- util: externalptr RAII ----------
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

// ==============================
  //  RCPP-ENTRYPOINTS
// ==============================
  
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
                       double start_qty_base) {
  Engine* e = engine_from_xp(xp);
  return e->eval_dirty(K, fee_pct, slip_base_frac, buf_base_frac,
                       k_slip, rvbps_fallback_bps, start_qty_base);
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
                      int    min_interval_fast_ms) {
  Engine* e = engine_from_xp(xp);
  return e->tick_once(K, fee_pct, slip_base_frac, buf_base_frac,
                      k_slip, rvbps_fallback_bps, start_qty_base,
                      near_window_frac, now_ms, min_interval_ms, min_interval_fast_ms);
}

// ===== Stand-alone helpers expected by evaluate_all() =================
  
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
      v = M(s0,  mid0);   // start -> mid
      m = M(mid0, end0);  // mid   -> end
      d = M(s0,  end0);   // start -> end
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
                        double fee_pct)
  {
    if (start_i_1b < 1) stop("tri_pnl_cpp: start index must be 1-based and >= 1");
    if (route_idx.nrow() < 1 || route_idx.ncol() < 2)
      stop("tri_pnl_cpp: route_idx must be at least one row and 2 columns");
    
    const int s0 = start_i_1b - 1;
    const int n  = route_idx.nrow();
    
    // Prealloker utdata (én rad per rute)
    LogicalVector ok(n);
    NumericVector final_base(n, NA_REAL), netPnL(n, NA_REAL), pnl_pct(n, NA_REAL);
    IntegerVector leg1_side(n, NA_INTEGER), leg2_side(n, NA_INTEGER), leg3_side(n, NA_INTEGER);
    
    for (int k = 0; k < n; ++k) {
      int mid1 = route_idx(k, 0);
      int end1 = route_idx(k, 1);
      
      if (mid1 <= 0 || end1 <= 0) { ok[k] = false; continue; }
      
      int mid0 = mid1 - 1;
      int end0 = end1 - 1;
      
      double amt = start_qty_base;
      int s1 = NA_INTEGER, s2 = NA_INTEGER, s3 = NA_INTEGER;
      
      // Legg 1: start -> mid (best-of-two)
      {
        double after = amt - fee_pct * amt;
        double c_rate;
        amt = convert_best(after, s0, mid0, BID, ASK, s1, c_rate);
        if (!R_finite(amt)) { ok[k]=false; leg1_side[k]=s1; continue; }
      }
      
      // Legg 2: mid -> end (best-of-two)
      {
        double after = amt - fee_pct * amt;
        double c_rate;
        amt = convert_best(after, mid0, end0, BID, ASK, s2, c_rate);
        if (!R_finite(amt)) { ok[k]=false; leg1_side[k]=s1; leg2_side[k]=s2; continue; }
      }
      
      // Legg 3: end -> start (best-of-two)
      {
        double after = amt - fee_pct * amt;
        double c_rate;
        amt = convert_best(after, end0, s0, BID, ASK, s3, c_rate);
        if (!R_finite(amt)) { ok[k]=false; leg1_side[k]=s1; leg2_side[k]=s2; leg3_side[k]=s3; continue; }
      }
      
      ok[k]         = true;
      final_base[k] = amt;
      const double pnl = amt - start_qty_base;
      netPnL[k]     = pnl;
      pnl_pct[k]    = (start_qty_base > 0.0) ? (100.0 * pnl / start_qty_base) : NA_REAL;
      leg1_side[k]  = s1; leg2_side[k] = s2; leg3_side[k] = s3;
    }
    
    return DataFrame::create(
      _["ok"]         = ok,
      _["final_base"] = final_base,
      _["netPnL"]     = netPnL,
      _["pnl_pct"]    = pnl_pct,
      _["leg1_side"]  = leg1_side,
      _["leg2_side"]  = leg2_side,
      _["leg3_side"]  = leg3_side,
      _["stringsAsFactors"] = false
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