#include "pair_update.hpp"
#include <cmath>

void update_one_pair_cpp(Rcpp::NumericMatrix &M,
                         Rcpp::NumericMatrix &BID,
                         Rcpp::NumericMatrix &ASK,
                         int i, int j,
                         double bid, double ask)
{
    BID(i, j) = bid;
    ASK(i, j) = ask;

    if (bid > 0 && ask > 0)
        M(i, j) = std::sqrt(bid * ask);
}