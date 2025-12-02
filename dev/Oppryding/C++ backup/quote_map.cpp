#include "engine_core.hpp"
#include <chrono>
#include <execution>

using namespace Rcpp;

// High-res clock
static inline uint64_t now_ns() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::steady_clock::now().time_since_epoch()
    ).count();
}

EngineCore::EngineCore(Rcpp::IntegerMatrix route_idx, int start_i_1b, int n_ccy_)
: n_ccy(n_ccy_), start0(start_i_1b - 1), routes(route_idx), running(false)
{
    size_t N = (size_t)n_ccy * (size_t)n_ccy * sizeof(double);

    M   = (double*)aligned_alloc(64, N);
    BID = (double*)aligned_alloc(64, N);
    ASK = (double*)aligned_alloc(64, N);

    memset(M,   0, N);
    memset(BID, 0, N);
    memset(ASK, 0, N);

    int R = routes.nrow();
    tmp_final.resize(R);
    tmp_edge.resize(R);
    tmp_pnl.resize(R);
}

inline void EngineCore::update_one_pair_accel(int idx, double bid, double ask) {
    BID[idx] = bid;
    ASK[idx] = ask;

    if (bid > 0 && ask > 0)
        M[idx] = sqrt(bid * ask);
}

inline double EngineCore::simulate_pnl(int mid, int end, double &final_base, double &edge_out)
{
    const double start_qty = 100000.0;

    double ask1 = ASK[start0 * n_ccy + mid];
    if (ask1 <= 0) return NA_REAL;
    double qty_mid = start_qty / ask1;

    double ask2 = ASK[mid * n_ccy + end];
    if (ask2 <= 0) return NA_REAL;
    double qty_end = qty_mid / ask2;

    double bid3 = BID[end * n_ccy + start0];
    if (bid3 <= 0) return NA_REAL;

    final_base = qty_end * bid3;
    double pnl = final_base - start_qty;
    edge_out = (final_base / start_qty) - 1.0;

    return pnl;
}

void EngineCore::eval_all_routes_accel()
{
    int R = routes.nrow();

    std::for_each(std::execution::par_unseq,
                  tmp_pnl.begin(),
                  tmp_pnl.end(),
                  [&](double &dummy)
    {
        size_t k = &dummy - &tmp_pnl[0];

        int mid = routes(k, 0) - 1;
        int end = routes(k, 1) - 1;

        __builtin_prefetch(&ASK[start0 * n_ccy + mid], 0, 1);
        __builtin_prefetch(&ASK[mid * n_ccy + end],   0, 1);
        __builtin_prefetch(&BID[end * n_ccy + start0],0, 1);

        double final_base = NA_REAL, edge = 0.0;
        double pnl = simulate_pnl(mid, end, final_base, edge);

        tmp_final[k] = final_base;
        tmp_edge[k]  = edge;
        tmp_pnl[k]   = pnl;

        if (std::isfinite(pnl)) {
            ResultItem r;
            r.mid_i = mid + 1;
            r.end_i = end + 1;
            r.final_base = final_base;
            r.edge = edge;
            r.pnl = pnl;
            r.pct = (pnl / 100000.0) * 100.0;
            r.ts_ns = now_ns();
            resultbuf.push(r);
        }
    });
}

bool EngineCore::push_tick(int i_1b, int j_1b, double bid, double ask)
{
    TickItem t{ i_1b, j_1b, bid, ask, now_ns() };
    return tickbuf.push(t);
}

void EngineCore::worker_loop()
{
    while (running.load()) {
        TickItem t;
        if (tickbuf.pop(t)) {
            int idx = (t.i_1b - 1) * n_ccy + (t.j_1b - 1);
            update_one_pair_accel(idx, t.bid, t.ask);
            eval_all_routes_accel();
        }
        std::this_thread::sleep_for(std::chrono::microseconds(200));
    }
}

void EngineCore::start_thread() {
    if (running.load()) return;
    running.store(true);
    worker = std::thread([this]{ worker_loop(); });
}

void EngineCore::stop_thread() {
    running.store(false);
    if (worker.joinable()) worker.join();
}

Rcpp::DataFrame EngineCore::pop_results()
{
    std::vector<int> mid, end;
    std::vector<double> pnl, edge, final_base, pct, ts;

    ResultItem r;
    while (resultbuf.pop(r)) {
        mid.push_back(r.mid_i);
        end.push_back(r.end_i);
        pnl.push_back(r.pnl);
        edge.push_back(r.edge);
        final_base.push_back(r.final_base);
        pct.push_back(r.pct);
        ts.push_back((double)r.ts_ns);
    }

    return Rcpp::DataFrame::create(
        _["mid"]=mid,
        _["end"]=end,
        _["pnl"]=pnl,
        _["edge"]=edge,
        _["final_base"]=final_base,
        _["pct"]=pct,
        _["ts_ns"]=ts
    );
}

// ==== R API ====
[[Rcpp::export]]
SEXP engine_create(Rcpp::IntegerMatrix routes, int start, int n)
{
    EngineCore* eng = new EngineCore(routes, start, n);
    return Rcpp::XPtr<EngineCore>(eng, true);
}

[[Rcpp::export]]
void engine_start(SEXP xp)
{
    Rcpp::XPtr<EngineCore> eng(xp);
    eng->start_thread();
}

[[Rcpp::export]]
void engine_stop(SEXP xp)
{
    Rcpp::XPtr<EngineCore> eng(xp);
    eng->stop_thread();
}

[[Rcpp::export]]
bool engine_push_tick(SEXP xp, int i, int j, double bid, double ask)
{
    Rcpp::XPtr<EngineCore> eng(xp);
    return eng->push_tick(i, j, bid, ask);
}

[[Rcpp::export]]
Rcpp::DataFrame engine_poll(SEXP xp)
{
    Rcpp::XPtr<EngineCore> eng(xp);
    return eng->pop_results();
}