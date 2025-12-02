#include "price_matrices.hpp"
#include <unordered_map>
#include <cmath>

PriceMatrices make_price_matrices_cpp(const std::vector<std::string> &ccys,
                                      const std::vector<std::string> &from,
                                      const std::vector<std::string> &to,
                                      const std::vector<double> &bid,
                                      const std::vector<double> &ask)
{
    int n = ccys.size();
    PriceMatrices pm{
        Rcpp::NumericMatrix(n, n),
        Rcpp::NumericMatrix(n, n),
        Rcpp::NumericMatrix(n, n)};

    std::unordered_map<std::string, int> idx;
    idx.reserve(n); // 🚀 viktig for performance

    for (int i = 0; i < n; i++)
        idx.emplace(ccys[i], i);

    int K = from.size();
    for (int k = 0; k < K; k++)
    {
        int i = idx[from[k]];
        int j = idx[to[k]];

        double b = bid[k];
        double a = ask[k];

        pm.BID(i, j) = b;
        pm.ASK(i, j) = a;

        if (b > 0 && a > 0)
            pm.M(i, j) = std::sqrt(b * a);
    }

    return pm;
}