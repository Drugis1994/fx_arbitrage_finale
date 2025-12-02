#pragma once
#include <Rcpp.h>
#include <string>
#include <vector>

struct QuoteMap
{
    std::vector<std::string> from;
    std::vector<std::string> to;
    std::vector<double> bid;
    std::vector<double> ask;
    std::vector<std::string> time;
};

QuoteMap build_quote_map_cpp(const Rcpp::CharacterVector &pairs,
                             const Rcpp::NumericVector &bid,
                             const Rcpp::NumericVector &ask,
                             const Rcpp::CharacterVector &time);