#pragma once
#include <Rcpp.h>
#include <vector>
#include <string>

struct PriceMatrices
{
    Rcpp::NumericMatrix M;
    Rcpp::NumericMatrix BID;
    Rcpp::NumericMatrix ASK;
};

PriceMatrices make_price_matrices_cpp(const std::vector<std::string> &ccys,
                                      const std::vector<std::string> &from,
                                      const std::vector<std::string> &to,
                                      const std::vector<double> &bid,
                                      const std::vector<double> &ask);