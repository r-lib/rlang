#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List make_lazy_sexp(SEXP prom) {
  if (TYPEOF(prom) != PROMSXP) {
    stop("Not a promise");
  }

  // recurse until we find the real promise, not a promise of a promise
  while(true) {
    SEXP code = PRCODE(prom);
    if(TYPEOF(code) != PROMSXP) break;
    prom = code;
  }

  List lazy = List::create(
    _["expr"] = PRCODE(prom),
    _["env"] = PRENV(prom)
  );
  lazy.attr("class") = "lazy";

  return lazy;
}

// [[Rcpp::export]]
RObject make_lazy_name_env(Symbol name, Environment env) {
  SEXP prom = Rf_findVar(name, env);
  return make_lazy_sexp(prom);
}
