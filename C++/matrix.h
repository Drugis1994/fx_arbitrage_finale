#pragma once
#include <stddef.h>
#include <math.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct matrix_t
    {
        int n;
        double *M;
        double *BID;
        double *ASK;
    } matrix_t;

    int matrix_init(matrix_t *m, int n);
    void matrix_free(matrix_t *m);

    static inline size_t matrix_idx(const matrix_t *m, int i0, int j0)
    {
        return (size_t)i0 * (size_t)m->n + (size_t)j0;
    }

    static inline void matrix_update_pair(matrix_t *m, int i0, int j0, double bid, double ask)
    {
        size_t k = matrix_idx(m, i0, j0);
        m->BID[k] = bid;
        m->ASK[k] = ask;

        if (bid > 0.0 && ask > 0.0)
        {
            double x = bid * ask;
            m->M[k] = (x > 0.0) ? sqrt(x) : 0.0;
        }
        else
        {
            m->M[k] = 0.0;
        }
    }

#ifdef __cplusplus
}
#endif