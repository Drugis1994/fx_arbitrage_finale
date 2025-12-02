#include "matrix.h"
#include <stdlib.h>
#include <string.h>

static void *aligned_malloc64(size_t bytes)
{
    void *p = NULL;
    // posix_memalign works well on macOS
    if (posix_memalign(&p, 64, bytes) != 0)
        return NULL;
    return p;
}

int matrix_init(matrix_t *m, int n)
{
    if (!m || n <= 1)
        return 0;
    m->n = n;

    size_t N = (size_t)n * (size_t)n;
    size_t bytes = N * sizeof(double);

    m->M = (double *)aligned_malloc64(bytes);
    m->BID = (double *)aligned_malloc64(bytes);
    m->ASK = (double *)aligned_malloc64(bytes);
    if (!m->M || !m->BID || !m->ASK)
    {
        matrix_free(m);
        return 0;
    }

    memset(m->M, 0, bytes);
    memset(m->BID, 0, bytes);
    memset(m->ASK, 0, bytes);
    return 1;
}

void matrix_free(matrix_t *m)
{
    if (!m)
        return;
    free(m->M);
    m->M = NULL;
    free(m->BID);
    m->BID = NULL;
    free(m->ASK);
    m->ASK = NULL;
    m->n = 0;
}