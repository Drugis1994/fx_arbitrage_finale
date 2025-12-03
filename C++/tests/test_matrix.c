#include "matrix.h"
#include <assert.h>
#include <stdio.h>

static void test_bounds_and_update(void)
{
    matrix_t m = {0};

    assert(matrix_init(&m, 3));

    assert(matrix_in_bounds(&m, 0, 0));
    assert(matrix_in_bounds(&m, 2, 2));
    assert(!matrix_in_bounds(&m, -1, 0));
    assert(!matrix_in_bounds(&m, 3, 1));

    assert(!matrix_update_pair(&m, -1, 0, 1.0, 1.0));
    assert(!matrix_update_pair(&m, 3, 0, 1.0, 1.0));

    assert(matrix_update_pair(&m, 1, 2, 1.5, 2.0));
    size_t idx = matrix_idx(&m, 1, 2);
    assert(m.BID[idx] == 1.5);
    assert(m.ASK[idx] == 2.0);
    assert(m.M[idx] > 0.0);

    matrix_free(&m);
}

int main(void)
{
    test_bounds_and_update();
    printf("matrix tests passed\n");
    return 0;
}
