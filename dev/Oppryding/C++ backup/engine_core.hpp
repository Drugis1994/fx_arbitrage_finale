#pragma once
#include <Rcpp.h>
#include <atomic>
#include <thread>
#include <vector>
#include <cstdint>
#include <array>
#include <Accelerate/Accelerate.h>
#include <execution>     // par_unseq
#include <cstdlib>       // aligned_alloc

// ===== Tick / Result =====
struct TickItem {
    int i_1b;
    int j_1b;
    double bid;
    double ask;
    uint64_t ts_ns;
};

struct ResultItem {
    int mid_i;
    int end_i;
    double edge;
    double final_base;
    double pnl;
    double pct;
    uint64_t ts_ns;
};

// ===== Lock-free RingBuffer =====
template <typename T, size_t N = 1024>
class RingBuffer {
public:
    RingBuffer() : head(0), tail(0) {}

    inline bool push(const T &v) {
        size_t h = head.load(std::memory_order_relaxed);
        size_t n = (h + 1) % N;
        if (n == tail.load(std::memory_order_acquire))
            return false;
        buffer[h] = v;
        head.store(n, std::memory_order_release);
        return true;
    }

    inline bool pop(T &out) {
        size_t t = tail.load(std::memory_order_relaxed);
        if (t == head.load(std::memory_order_acquire))
            return false;
        out = buffer[t];
        tail.store((t + 1) % N, std::memory_order_release);
        return true;
    }

private:
    std::array<T, N> buffer;
    std::atomic<size_t> head;
    std::atomic<size_t> tail;
};

// ===== ENGINE CORE V3.5 (Accelerate + Parallel) =====
class EngineCore {
public:
    EngineCore(Rcpp::IntegerMatrix route_idx, int start_i_1b, int n_ccy_);
    void start_thread();
    void stop_thread();
    bool push_tick(int i_1b, int j_1b, double bid, double ask);
    Rcpp::DataFrame pop_results();

private:
    void worker_loop();
    inline void update_one_pair_accel(int idx, double bid, double ask);
    inline double simulate_pnl(int mid, int end, double &final_base, double &edge_out);
    void eval_all_routes_accel();

private:
    int n_ccy;
    int start0;
    Rcpp::IntegerMatrix routes;

    // Flattened aligned matrices
    double* M;
    double* BID;
    double* ASK;

    // Thread control
    std::atomic<bool> running;
    std::thread worker;

    // Buffers
    RingBuffer<TickItem,1024> tickbuf;
    RingBuffer<ResultItem,1024> resultbuf;

    // temp
    std::vector<double> tmp_final;
    std::vector<double> tmp_edge;
    std::vector<double> tmp_pnl;
};