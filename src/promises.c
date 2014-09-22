#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>

SEXP Rf_mkPROMISE(SEXP, SEXP);

SEXP promise_(SEXP expr, SEXP env) {
  if (TYPEOF(expr) != SYMSXP && TYPEOF(expr) != LANGSXP) {
    error("expr must be a call or a symbol");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("env must be an environment");
  }

  return Rf_mkPROMISE(expr, env);
}

SEXP promise_expr_(SEXP prom) {
  if (TYPEOF(prom) != PROMSXP) {
    error("prom must be a promise");
  }

  return PREXPR(prom);
}

SEXP promise_env_(SEXP prom) {
  if (TYPEOF(prom) != PROMSXP) {
    error("prom must be a promise");
  }

  return PRENV(prom);
}
