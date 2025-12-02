#include "engine_participation.hpp"
#include <cmath>

Rcpp::LogicalVector engine_rebuild_participation_cpp(
    const Rcpp::NumericMatrix &M)
{
    int n = M.nrow();
    Rcpp::LogicalVector active(n * n, false);

    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
        {
            double x = M(i, j);
            if (i != j && x > 0 && std::isfinite(x))
                active[i * n + j] = true;
        }

    return active;
}