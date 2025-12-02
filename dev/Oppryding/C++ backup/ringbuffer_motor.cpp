// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <atomic>
#include <pthread.h>
#include <sched.h>
#include <unistd.h>
#include <time.h>
#include <chrono>

#include "engine_core.hpp"

using namespace Rcpp;

// ============================================================
// GLOBAL ENGINE POINTER (C++ raw pointer from XPtr)
// ============================================================
static EngineCore *ENG = nullptr;

// ============================================================
// HIGH-RES CLOCK
// ============================================================
static inline uint64_t now_ns()
{
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
               std::chrono::steady_clock::now().time_since_epoch())
        .count();
}

// ============================================================
// THREAD CONTROL
// ============================================================
static std::atomic<bool> running(false);
static pthread_t motor_thread;

// ============================================================
// CPU PINNING
// ============================================================
static int best_perf_core()
{
#ifdef __APPLE__
    return sysconf(_SC_NPROCESSORS_CONF) - 1;
#else
    return 0;
#endif
}

// ============================================================
// PUSH TICK FROM R
// ============================================================

// [[Rcpp::export]]
bool motor_push_tick(int i_1b, int j_1b, double bid, double ask)
{
    if (!ENG)
        return false;
    return ENG->push_tick(i_1b, j_1b, bid, ask);
}

// ============================================================
// POLL RESULTS BACK TO R
// ============================================================

// [[Rcpp::export]]
DataFrame motor_poll_results()
{
    if (!ENG)
        return DataFrame::create();
    return ENG->pop_results();
}

// ============================================================
// WORKER LOOP
// ============================================================
static void *motor_loop(void *arg)
{

    cpu_set_t cs;
    CPU_ZERO(&cs);
    CPU_SET(best_perf_core(), &cs);
    pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cs);

    struct sched_param sp;
    sp.sched_priority = 80;
    pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp);

    while (running.load(std::memory_order_acquire))
    {
        ENG->worker_loop();
    }

    return nullptr;
}

// ============================================================
// START MOTOR
// ============================================================

// [[Rcpp::export]]
void motor_start(SEXP xp)
{
    if (running.load())
        return;

    // Correct XPtr -> raw pointer extraction
    ENG = Rcpp::XPtr<EngineCore>(xp).get();

    running.store(true);
    pthread_create(&motor_thread, nullptr, motor_loop, nullptr);
}

// ============================================================
// STOP MOTOR
// ============================================================

// [[Rcpp::export]]
void motor_stop()
{
    running.store(false);
    pthread_join(motor_thread, nullptr);
}