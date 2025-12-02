#pragma once
#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum routes_kind_t
    {
        ROUTES_KIND_TRI = 1,  // routes are (mid,end)
        ROUTES_KIND_CYCLE = 2 // general cycle nodes (N legs)
    } routes_kind_t;

    typedef struct routes_t
    {
        routes_kind_t kind;
        int n_routes;

        // TRI fast-path:
        int *mid0; // 0-based
        int *end0; // 0-based

        // General cycle:
        int n_cols;  // nodes per route row (>=2). legs = n_cols-1
        int *nodes0; // row-major, 0-based indices, length n_routes*n_cols
    } routes_t;

    void routes_free(routes_t *r);

    // Create TRI from mid/end row-major.
    // input_mid_end: length n_routes*2
    int routes_init_tri(routes_t *r, const int *input_mid_end, int n_routes, int one_based);

    // Create cycle from nodes row-major.
    // nodes: length n_routes*n_cols; columns = nodes per route (n_legs+1)
    // If one_based=1, converts to 0-based.
    int routes_init_cycle(routes_t *r, const int *nodes, int n_routes, int n_cols, int one_based);

#ifdef __cplusplus
}
#endif