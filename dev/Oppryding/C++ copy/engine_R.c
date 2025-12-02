#include <R.h>
#include <Rinternals.h>
#include <stdint.h>
#include <stdlib.h>

#include "engine.h"
#include "results.h"

// finalizer
static void engine_xptr_finalizer(SEXP xptr)
{
    engine_t *eng = (engine_t *)R_ExternalPtrAddr(xptr);
    if (eng)
    {
        engine_stop(eng);
        engine_destroy(eng);
        R_ClearExternalPtr(xptr);
    }
}

// R integer matrix is column-major; convert to row-major int*
static int *int_matrix_to_rowmajor(SEXP mat, int *out_nrow, int *out_ncol)
{
    if (!isInteger(mat) || !isMatrix(mat))
        error("routes must be an integer matrix");

    SEXP dim = getAttrib(mat, R_DimSymbol);
    int nrow = INTEGER(dim)[0];
    int ncol = INTEGER(dim)[1];
    if (nrow <= 0 || ncol <= 0)
        error("routes has invalid dimensions");

    int *src = INTEGER(mat);
    int *dst = (int *)malloc((size_t)nrow * (size_t)ncol * sizeof(int));
    if (!dst)
        error("malloc failed");

    for (int r = 0; r < nrow; ++r)
    {
        for (int c = 0; c < ncol; ++c)
        {
            dst[(size_t)r * (size_t)ncol + (size_t)c] = src[r + nrow * c];
        }
    }

    *out_nrow = nrow;
    *out_ncol = ncol;
    return dst;
}

// .Call engine_create(routes, start_i_1b, n_ccy)
// routes:
//   - if ncol==2 => TRI routes (mid,end) 1-based
//   - if ncol>=3 => cycle nodes (N legs) 1-based
SEXP engine_create(SEXP routes, SEXP start_i_1b, SEXP n_ccy)
{
    if (!isInteger(start_i_1b) || LENGTH(start_i_1b) != 1)
        error("start_i_1b must be integer(1)");
    if (!isInteger(n_ccy) || LENGTH(n_ccy) != 1)
        error("n_ccy must be integer(1)");

    int start = INTEGER(start_i_1b)[0];
    int n = INTEGER(n_ccy)[0];

    int nrow = 0, ncol = 0;
    int *rowmajor = int_matrix_to_rowmajor(routes, &nrow, &ncol);

    engine_t *eng = NULL;
    if (ncol == 2)
    {
        eng = engine_create_tri(n, start, rowmajor, nrow, /*one_based=*/1);
    }
    else if (ncol >= 3)
    {
        eng = engine_create_cycle(n, start, rowmajor, nrow, ncol, /*one_based=*/1);
    }
    else
    {
        free(rowmajor);
        error("routes must have at least 2 columns");
    }

    free(rowmajor);
    if (!eng)
        error("engine_create failed");

    SEXP xptr = PROTECT(R_MakeExternalPtr((void *)eng, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(xptr, engine_xptr_finalizer, 1);
    UNPROTECT(1);
    return xptr;
}

SEXP engine_start_R(SEXP xptr)
{
    engine_t *eng = (engine_t *)R_ExternalPtrAddr(xptr);
    if (!eng)
        error("engine ptr is null");
    return ScalarLogical(engine_start(eng) ? 1 : 0);
}

SEXP engine_stop_R(SEXP xptr)
{
    engine_t *eng = (engine_t *)R_ExternalPtrAddr(xptr);
    if (!eng)
        return R_NilValue;
    engine_stop(eng);
    return R_NilValue;
}

SEXP engine_push_tick_R(SEXP xptr, SEXP i_1b, SEXP j_1b, SEXP bid, SEXP ask)
{
    engine_t *eng = (engine_t *)R_ExternalPtrAddr(xptr);
    if (!eng)
        error("engine ptr is null");

    if (!isInteger(i_1b) || LENGTH(i_1b) != 1)
        error("i_1b must be integer(1)");
    if (!isInteger(j_1b) || LENGTH(j_1b) != 1)
        error("j_1b must be integer(1)");
    if (!isReal(bid) || LENGTH(bid) != 1)
        error("bid must be double(1)");
    if (!isReal(ask) || LENGTH(ask) != 1)
        error("ask must be double(1)");

    int i = INTEGER(i_1b)[0];
    int j = INTEGER(j_1b)[0];
    double b = REAL(bid)[0];
    double a = REAL(ask)[0];

    return ScalarLogical(engine_push_tick(eng, i, j, b, a) ? 1 : 0);
}

SEXP engine_poll(SEXP xptr)
{
    engine_t *eng = (engine_t *)R_ExternalPtrAddr(xptr);
    if (!eng)
        error("engine ptr is null");

    // dynamic accumulate
    size_t cap = 1024, n = 0;
    int *route_id = (int *)malloc(cap * sizeof(int));
    int *mid_i = (int *)malloc(cap * sizeof(int));
    int *end_i = (int *)malloc(cap * sizeof(int));
    double *pnl = (double *)malloc(cap * sizeof(double));
    double *edge = (double *)malloc(cap * sizeof(double));
    double *fin = (double *)malloc(cap * sizeof(double));
    double *pct = (double *)malloc(cap * sizeof(double));
    double *ts = (double *)malloc(cap * sizeof(double));

    if (!route_id || !mid_i || !end_i || !pnl || !edge || !fin || !pct || !ts)
    {
        free(route_id);
        free(mid_i);
        free(end_i);
        free(pnl);
        free(edge);
        free(fin);
        free(pct);
        free(ts);
        error("malloc failed");
    }

    result_t r;
    while (engine_pop_result(eng, &r))
    {
        if (n == cap)
        {
            cap *= 2;
            route_id = (int *)realloc(route_id, cap * sizeof(int));
            mid_i = (int *)realloc(mid_i, cap * sizeof(int));
            end_i = (int *)realloc(end_i, cap * sizeof(int));
            pnl = (double *)realloc(pnl, cap * sizeof(double));
            edge = (double *)realloc(edge, cap * sizeof(double));
            fin = (double *)realloc(fin, cap * sizeof(double));
            pct = (double *)realloc(pct, cap * sizeof(double));
            ts = (double *)realloc(ts, cap * sizeof(double));
            if (!route_id || !mid_i || !end_i || !pnl || !edge || !fin || !pct || !ts)
                error("realloc failed");
        }
        route_id[n] = r.route_id;
        mid_i[n] = r.mid_i;
        end_i[n] = r.end_i;
        pnl[n] = r.pnl;
        edge[n] = r.edge;
        fin[n] = r.final_base;
        pct[n] = r.pct;
        ts[n] = (double)r.ts_ns;
        n++;
    }

    SEXP Route = PROTECT(allocVector(INTSXP, (R_xlen_t)n));
    SEXP Mid = PROTECT(allocVector(INTSXP, (R_xlen_t)n));
    SEXP End = PROTECT(allocVector(INTSXP, (R_xlen_t)n));
    SEXP Pnl = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
    SEXP Edge = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
    SEXP Fin = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
    SEXP Pct = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
    SEXP Ts = PROTECT(allocVector(REALSXP, (R_xlen_t)n));

    for (size_t i = 0; i < n; ++i)
    {
        INTEGER(Route)
        [i] = route_id[i];
        INTEGER(Mid)
        [i] = mid_i[i];
        INTEGER(End)
        [i] = end_i[i];
        REAL(Pnl)
        [i] = pnl[i];
        REAL(Edge)
        [i] = edge[i];
        REAL(Fin)
        [i] = fin[i];
        REAL(Pct)
        [i] = pct[i];
        REAL(Ts)
        [i] = ts[i];
    }

    free(route_id);
    free(mid_i);
    free(end_i);
    free(pnl);
    free(edge);
    free(fin);
    free(pct);
    free(ts);

    SEXP df = PROTECT(allocVector(VECSXP, 8));
    SET_VECTOR_ELT(df, 0, Route);
    SET_VECTOR_ELT(df, 1, Mid);
    SET_VECTOR_ELT(df, 2, End);
    SET_VECTOR_ELT(df, 3, Pnl);
    SET_VECTOR_ELT(df, 4, Edge);
    SET_VECTOR_ELT(df, 5, Fin);
    SET_VECTOR_ELT(df, 6, Pct);
    SET_VECTOR_ELT(df, 7, Ts);

    SEXP names = PROTECT(allocVector(STRSXP, 8));
    SET_STRING_ELT(names, 0, mkChar("route_id"));
    SET_STRING_ELT(names, 1, mkChar("mid"));
    SET_STRING_ELT(names, 2, mkChar("end"));
    SET_STRING_ELT(names, 3, mkChar("pnl"));
    SET_STRING_ELT(names, 4, mkChar("edge"));
    SET_STRING_ELT(names, 5, mkChar("final_base"));
    SET_STRING_ELT(names, 6, mkChar("pct"));
    SET_STRING_ELT(names, 7, mkChar("ts_ns"));
    setAttrib(df, R_NamesSymbol, names);

    SEXP rownames = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rownames)
    [0] = NA_INTEGER;
    INTEGER(rownames)
    [1] = (int)n;
    setAttrib(df, R_RowNamesSymbol, rownames);

    setAttrib(df, R_ClassSymbol, mkString("data.frame"));

    UNPROTECT(11);
    return df;
}