#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List make_lazy(Symbol name, Environment env) {
  SEXP promise = Rf_findVar(name, env);

  // recurse until we find the real promise, not a promise of a promise
  while(TYPEOF(promise) == PROMSXP) {
    env = PRENV(promise);
    promise = PREXPR(promise);

    // If the promise is threaded through multiple functions, we'll
    // get some symbols along the way. If the symbol is bound to a promise
    // keep going on up
    if (TYPEOF(promise) == SYMSXP) {
      SEXP obj = Rf_findVar(promise, env);
      if (TYPEOF(obj) == PROMSXP) {
        promise = obj;
      }
    }
  }

  List lazy = List::create(
    _["expr"] = promise,
    _["env"] = env
  );
  lazy.attr("class") = "lazy";

  return lazy;
}
