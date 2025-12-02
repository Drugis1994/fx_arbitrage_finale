#include "ticks.h"

#if defined(__APPLE__)
#include <mach/mach_time.h>
uint64_t ticks_now_ns(void)
{
    static mach_timebase_info_data_t tb = {0, 0};
    if (tb.denom == 0)
        (void)mach_timebase_info(&tb);
    uint64_t t = mach_absolute_time();
    // ns = t * numer/denom
    return (t * (uint64_t)tb.numer) / (uint64_t)tb.denom;
}
#else
#include <time.h>
uint64_t ticks_now_ns(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}
#endif