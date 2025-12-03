#include "matrix.h"

#include <assert.h>
#include <stdio.h>

static void test_matrix_bounds(void)
{
    matrix_t m = {0};
    assert(!matrix_in_bounds(NULL, 0, 0));
    assert(!matrix_in_bounds(&m, 0, 0));

    assert(matrix_init(&m, 3));
    assert(matrix_in_bounds(&m, 0, 0));
    assert(matrix_in_bounds(&m, 2, 2));
    assert(!matrix_in_bounds(&m, -1, 0));
    assert(!matrix_in_bounds(&m, 3, 0));
    matrix_free(&m);
}

static void test_matrix_update_pair(void)
{
    matrix_t m = {0};
    assert(matrix_init(&m, 2));

    /* invalid inputs */
    assert(!matrix_update_pair(&m, -1, 0, 1.0, 1.0));
    assert(!matrix_update_pair(&m, 0, 2, 1.0, 1.0));
    assert(!matrix_update_pair(&m, 0, 1, 1.0, -1.0));

    /* valid update */
    assert(matrix_update_pair(&m, 0, 1, 1.1, 1.2));
    size_t idx = matrix_idx(&m, 0, 1);
    assert(m.BID[idx] == 1.1);
    assert(m.ASK[idx] == 1.2);
    assert(m.M[idx] > 0.0);

    matrix_free(&m);
}

int main(void)
{
    test_matrix_bounds();
    test_matrix_update_pair();
    printf("matrix tests passed\n");
    return 0;
}
