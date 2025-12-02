// [[Rcpp::export]]
List build_quote_map_cpp(CharacterVector pairs,
                         NumericVector bid,
                         NumericVector ask,
                         CharacterVector time) {

  int n = pairs.size();
  std::vector<std::string> from(n), to(n);

  for (int i = 0; i < n; ++i) {
    std::string s = Rcpp::as<std::string>(pairs[i]);
    auto pos = s.find("/");
    if (pos == std::string::npos) {
      from[i] = ""; 
      to[i]   = "";
    } else {
      from[i] = s.substr(0, pos);
      to[i]   = s.substr(pos+1);
    }
  }

  return Rcpp::List::create(
    _["from"] = from,
    _["to"]   = to,
    _["bid"]  = bid,
    _["ask"]  = ask,
    _["time"] = time
  );
}


// [[Rcpp::export]]
List make_price_matrices_cpp(CharacterVector ccys,
                             CharacterVector from,
                             CharacterVector to,
                             NumericVector bid,
                             NumericVector ask) {

  int n = ccys.size();
  int m = from.size();

  Rcpp::NumericMatrix M(n, n);
  Rcpp::NumericMatrix BID(n, n);
  Rcpp::NumericMatrix ASK(n, n);

  std::unordered_map<std::string,int> idx;
  idx.reserve(n);

  for (int i = 0; i < n; ++i)
    idx[Rcpp::as<std::string>(ccys[i])] = i;

  for (int k = 0; k < m; ++k) {
    std::string a = Rcpp::as<std::string>(from[k]);
    std::string b = Rcpp::as<std::string>(to[k]);
    if (!idx.count(a) || !idx.count(b)) continue;

    int i = idx[a];
    int j = idx[b];

    double B = bid[k];
    double A = ask[k];

    BID(i,j) = B;
    ASK(i,j) = A;

    if (R_finite(B) && B > 0)
      M(i,j) = B;
    else if (R_finite(A) && A > 0)
      M(i,j) = 1.0 / A;
  }

  return List::create(
    _["M"]   = M,
    _["BID"] = BID,
    _["ASK"] = ASK
  );
}

// [[Rcpp::export]]
void update_one_pair_cpp(SEXP xp,
                         int i_1b, int j_1b,
                         double bid, double ask,
                         double tol = 1e-12) {

  Engine* e = engine_from_xp(xp);

  int i = i_1b - 1;
  int j = j_1b - 1;
  if (i < 0 || j < 0 || i >= e->n_ccy || j >= e->n_ccy)
    return;

  double oldM = e->M(i,j);
  double newM = NA_REAL;

  if (R_finite(bid) && bid > 0)
    newM = bid;
  else if (R_finite(ask) && ask > 0)
    newM = 1.0 / ask;

  e->BID(i,j) = bid;
  e->ASK(i,j) = ask;
  e->M(i,j)   = newM;

  if (!R_finite(oldM) || !R_finite(newM) ||
      std::fabs(oldM - newM) > tol)
    e->mark_cell(i_1b, j_1b);
}