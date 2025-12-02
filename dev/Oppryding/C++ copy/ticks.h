#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct tick_t
    {
        int i_1b;
        int j_1b;
        double bid;
        double ask;
        uint64_t ts_ns;
    } tick_t;

    uint64_t ticks_now_ns(void);

#ifdef __cplusplus
}
#endif