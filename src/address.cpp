#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string address(SEXP x) {
  std::ostringstream s;
  s << x;
  return s.str();
}
