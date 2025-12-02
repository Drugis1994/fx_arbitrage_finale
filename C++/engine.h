#pragma once
#include <stdint.h>
#include "ticks.h"
#include "results.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct engine_t engine_t;

    // Constructors
    engine_t *engine_create_tri(int n_ccy, int start_i_1b,
                                const int *mid_end_rowmajor, int n_routes,
                                int one_based);

    engine_t *engine_create_cycle(int n_ccy, int start_i_1b,
                                  const int *nodes_rowmajor, int n_routes, int n_cols,
                                  int one_based);

    // Lifecycle
    void engine_destroy(engine_t *e);

    // Thread control
    int engine_start(engine_t *e);
    void engine_stop(engine_t *e);

    // Feed ticks
    int engine_push_tick(engine_t *e, int i_1b, int j_1b, double bid, double ask);

    // Pop results (non-blocking): returns 1 if popped, else 0
    int engine_pop_result(engine_t *e, result_t *out);

#ifdef __cplusplus
}
#endif