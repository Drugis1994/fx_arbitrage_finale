#include "routes.h"
#include <stdlib.h>
#include <string.h>

void routes_free(routes_t *r)
{
    if (!r)
        return;
    free(r->mid0);
    r->mid0 = NULL;
    free(r->end0);
    r->end0 = NULL;
    free(r->nodes0);
    r->nodes0 = NULL;
    r->n_routes = 0;
    r->n_cols = 0;
    r->kind = 0;
}

int routes_init_tri(routes_t *r, const int *input_mid_end, int n_routes, int one_based)
{
    if (!r || !input_mid_end || n_routes <= 0)
        return 0;
    memset(r, 0, sizeof(*r));
    r->kind = ROUTES_KIND_TRI;
    r->n_routes = n_routes;

    r->mid0 = (int *)malloc((size_t)n_routes * sizeof(int));
    r->end0 = (int *)malloc((size_t)n_routes * sizeof(int));
    if (!r->mid0 || !r->end0)
    {
        routes_free(r);
        return 0;
    }

    for (int k = 0; k < n_routes; ++k)
    {
        int mid = input_mid_end[2 * k + 0];
        int end = input_mid_end[2 * k + 1];
        if (one_based)
        {
            mid--;
            end--;
        }
        r->mid0[k] = mid;
        r->end0[k] = end;
    }
    return 1;
}

int routes_init_cycle(routes_t *r, const int *nodes, int n_routes, int n_cols, int one_based)
{
    if (!r || !nodes || n_routes <= 0 || n_cols < 2)
        return 0;
    memset(r, 0, sizeof(*r));
    r->kind = ROUTES_KIND_CYCLE;
    r->n_routes = n_routes;
    r->n_cols = n_cols;

    size_t L = (size_t)n_routes * (size_t)n_cols;
    r->nodes0 = (int *)malloc(L * sizeof(int));
    if (!r->nodes0)
    {
        routes_free(r);
        return 0;
    }

    for (size_t i = 0; i < L; ++i)
    {
        int v = nodes[i];
        if (one_based)
            v--;
        r->nodes0[i] = v;
    }
    return 1;
}