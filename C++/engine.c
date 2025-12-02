#include "engine.h"
#include "matrix.h"
#include "ringbuffer.h"
#include "routes.h"

#include <pthread.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

// Fixed ring sizes (A: fixed lock-free ringbuffer)
RB_DEFINE(tick_rb_t, tick_t, 8192)
RB_DEFINE(result_rb_t, result_t, 8192)

struct engine_t
{
    int n_ccy;
    int start0; // 0-based

    matrix_t mat;
    routes_t routes;

    // Threading
    _Atomic int running;
    pthread_t thread;

    // Buffers
    tick_rb_t tick_rb;
    result_rb_t result_rb;

    // Telemetry
    uint64_t ticks_received;
    uint64_t ticks_dropped;
    uint64_t results_emitted;
    uint64_t results_dropped;

    // scratch (avoid malloc each eval)
    double start_qty;
};

// ---------- Engine core math ----------
// TRI semantics matches din gamle engine:
// qty_mid = start_qty / ASK[start, mid]
// qty_end = qty_mid / ASK[mid, end]
// final   = qty_end * BID[end, start]
static inline int tri_simulate(const matrix_t *mat, int start0, int mid0, int end0,
                               double start_qty,
                               double *final_base, double *edge_out, double *pnl_out)
{
    size_t idx1 = matrix_idx(mat, start0, mid0);
    size_t idx2 = matrix_idx(mat, mid0, end0);
    size_t idx3 = matrix_idx(mat, end0, start0);

    double ask1 = mat->ASK[idx1];
    if (!(ask1 > 0.0) || !isfinite(ask1))
        return 0;
    double qty_mid = start_qty / ask1;

    double ask2 = mat->ASK[idx2];
    if (!(ask2 > 0.0) || !isfinite(ask2))
        return 0;
    double qty_end = qty_mid / ask2;

    double bid3 = mat->BID[idx3];
    if (!(bid3 > 0.0) || !isfinite(bid3))
        return 0;

    double fin = qty_end * bid3;
    double pnl = fin - start_qty;
    double edge = (fin / start_qty) - 1.0;

    *final_base = fin;
    *pnl_out = pnl;
    *edge_out = edge;
    return isfinite(pnl) ? 1 : 0;
}

static void eval_all_routes(engine_t *e)
{
    const int R = e->routes.n_routes;
    const uint64_t ts = ticks_now_ns();

    if (e->routes.kind == ROUTES_KIND_TRI)
    {
        int k = 0;
        for (; k + 3 < R; k += 4)
        {
            for (int u = 0; u < 4; ++u)
            {
                int kk = k + u;
                int mid0 = e->routes.mid0[kk];
                int end0 = e->routes.end0[kk];

                double fin, edge, pnl;
                if (tri_simulate(&e->mat, e->start0, mid0, end0, e->start_qty, &fin, &edge, &pnl))
                {
                    result_t r;
                    r.route_id = kk + 1;
                    r.mid_i = mid0 + 1;
                    r.end_i = end0 + 1;
                    r.final_base = fin;
                    r.edge = edge;
                    r.pnl = pnl;
                    r.pct = (pnl / e->start_qty) * 100.0;
                    r.ts_ns = ts;
                    if (result_rb_t_push(&e->result_rb, &r))
                    {
                        e->results_emitted++;
                    }
                    else
                    {
                        e->results_dropped++;
                    }
                }
            }
        }
        for (; k < R; ++k)
        {
            int mid0 = e->routes.mid0[k];
            int end0 = e->routes.end0[k];

            double fin, edge, pnl;
            if (tri_simulate(&e->mat, e->start0, mid0, end0, e->start_qty, &fin, &edge, &pnl))
            {
                result_t r;
                r.route_id = k + 1;
                r.mid_i = mid0 + 1;
                r.end_i = end0 + 1;
                r.final_base = fin;
                r.edge = edge;
                r.pnl = pnl;
                r.pct = (pnl / e->start_qty) * 100.0;
                r.ts_ns = ts;
                if (result_rb_t_push(&e->result_rb, &r))
                {
                    e->results_emitted++;
                }
                else
                {
                    e->results_dropped++;
                }
            }
        }
        return;
    }

    if (e->routes.kind == ROUTES_KIND_CYCLE)
    {
        const int n_cols = e->routes.n_cols;
        const int n_legs = n_cols - 1;

        for (int route = 0; route < R; ++route)
        {
            const int *row = &e->routes.nodes0[(size_t)route * (size_t)n_cols];

            double qty = e->start_qty;
            int ok = 1;

            for (int leg = 0; leg < n_legs - 1; ++leg)
            {
                int a = row[leg];
                int b = row[leg + 1];
                double ask = e->mat.ASK[matrix_idx(&e->mat, a, b)];
                if (!(ask > 0.0) || !isfinite(ask))
                {
                    ok = 0;
                    break;
                }
                qty = qty / ask;
            }
            if (!ok)
                continue;

            int last = row[n_legs - 1];
            int back = row[n_legs];
            double bid = e->mat.BID[matrix_idx(&e->mat, last, back)];
            if (!(bid > 0.0) || !isfinite(bid))
                continue;

            double fin = qty * bid;
            double pnl = fin - e->start_qty;
            if (!isfinite(pnl))
                continue;

            result_t r;
            r.route_id = route + 1;
            r.mid_i = 0;
            r.end_i = 0;
            r.final_base = fin;
            r.edge = (fin / e->start_qty) - 1.0;
            r.pnl = pnl;
            r.pct = (pnl / e->start_qty) * 100.0;
            r.ts_ns = ts;
            if (result_rb_t_push(&e->result_rb, &r))
            {
                e->results_emitted++;
            }
            else
            {
                e->results_dropped++;
            }
        }
    }
}

static void *engine_thread_main(void *p)
{
    engine_t *e = (engine_t *)p;

    while (atomic_load(&e->running))
    {
        tick_t t;
        int got = tick_rb_t_pop(&e->tick_rb, &t);
        if (!got)
        {
            struct timespec req;
            req.tv_sec = 0;
            req.tv_nsec = 200000; // 0.2 ms
            nanosleep(&req, NULL);
            continue;
        }

        int i0 = t.i_1b - 1;
        int j0 = t.j_1b - 1;
        matrix_update_pair(&e->mat, i0, j0, t.bid, t.ask);

        while (tick_rb_t_pop(&e->tick_rb, &t))
        {
            i0 = t.i_1b - 1;
            j0 = t.j_1b - 1;
            matrix_update_pair(&e->mat, i0, j0, t.bid, t.ask);
        }

        eval_all_routes(e);
    }
    return NULL;
}

// ---------- Public API ----------
static engine_t *engine_alloc_common(int n_ccy, int start_i_1b)
{
    if (n_ccy <= 1)
        return NULL;
    if (start_i_1b < 1 || start_i_1b > n_ccy)
        return NULL;

    engine_t *e = (engine_t *)calloc(1, sizeof(engine_t));
    if (!e)
        return NULL;

    e->n_ccy = n_ccy;
    e->start0 = start_i_1b - 1;
    e->start_qty = 100000.0;

    e->ticks_received  = 0;
    e->ticks_dropped   = 0;
    e->results_emitted = 0;
    e->results_dropped = 0;

    if (!matrix_init(&e->mat, n_ccy))
    {
        free(e);
        return NULL;
    }

    tick_rb_t_init(&e->tick_rb);
    result_rb_t_init(&e->result_rb);
    atomic_store(&e->running, 0);
    return e;
}

engine_t *engine_create_tri(int n_ccy, int start_i_1b,
                            const int *mid_end_rowmajor, int n_routes,
                            int one_based)
{
    engine_t *e = engine_alloc_common(n_ccy, start_i_1b);
    if (!e)
        return NULL;

    if (!routes_init_tri(&e->routes, mid_end_rowmajor, n_routes, one_based))
    {
        engine_destroy(e);
        return NULL;
    }
    return e;
}

engine_t *engine_create_cycle(int n_ccy, int start_i_1b,
                              const int *nodes_rowmajor, int n_routes, int n_cols,
                              int one_based)
{
    engine_t *e = engine_alloc_common(n_ccy, start_i_1b);
    if (!e)
        return NULL;

    if (!routes_init_cycle(&e->routes, nodes_rowmajor, n_routes, n_cols, one_based))
    {
        engine_destroy(e);
        return NULL;
    }
    return e;
}

void engine_destroy(engine_t *e)
{
    if (!e)
        return;
    engine_stop(e);
    routes_free(&e->routes);
    matrix_free(&e->mat);
    free(e);
}

int engine_start(engine_t *e)
{
    if (!e)
        return 0;

    int expected = 0;
    if (!atomic_compare_exchange_strong(&e->running, &expected, 1))
        return 0;

    if (pthread_create(&e->thread, NULL, engine_thread_main, e) != 0)
    {
        atomic_store(&e->running, 0); // viktig fix
        return 0;
    }
    return 1;
}

void engine_stop(engine_t *e)
{
    if (!e)
        return;
    if (atomic_exchange(&e->running, 0))
    {
        (void)pthread_join(e->thread, NULL);
    }
}

int engine_push_tick(engine_t *e, int i_1b, int j_1b, double bid, double ask)
{
    if (!e)
        return 0;

    tick_t t;
    t.i_1b = i_1b;
    t.j_1b = j_1b;
    t.bid = bid;
    t.ask = ask;
    t.ts_ns = ticks_now_ns();

    int ok = tick_rb_t_push(&e->tick_rb, &t);
    if (ok) {
        e->ticks_received++;
    } else {
        e->ticks_dropped++;
    }
    return ok;
}
int engine_pop_result(engine_t *e, result_t *out)
{
    if (!e || !out)
        return 0;
    return result_rb_t_pop(&e->result_rb, out);
}