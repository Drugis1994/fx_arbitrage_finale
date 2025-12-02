#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct result_t
    {
        int route_id; // 1-based
        int mid_i;    // 1-based (for tri routes; 0 if N-leg not representable)
        int end_i;    // 1-based (for tri routes; 0 if N-leg not representable)
        double edge;
        double final_base;
        double pnl;
        double pct;
        uint64_t ts_ns;
    } result_t;

#ifdef __cplusplus
}
#endif