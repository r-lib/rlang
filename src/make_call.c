#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>

// This is a bit naughty, but there's no other way to create a promise
SEXP Rf_mkPROMISE(SEXP, SEXP);
SEXP Rf_installTrChar(SEXP);

SEXP lazy_to_promise(SEXP x) {
  // arg is a list of length 2 - LANGSXP/SYMSXP, followed by ENVSXP
  return Rf_mkPROMISE(VECTOR_ELT(x, 0), VECTOR_ELT(x, 1));
}

SEXP eval_call_(SEXP fun, SEXP dots, SEXP env) {
  if (TYPEOF(fun) != SYMSXP && TYPEOF(fun) != LANGSXP) {
    error("fun must be a call or a symbol");
  }
  if (TYPEOF(dots) != VECSXP) {
    error("dots must be a list");
  }
  if (!inherits(dots, "lazy_dots")) {
    error("dots must be of class lazy_dots");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("env must be an environment");
  }

  int n = length(dots);
  if (n == 0) {
    return LCONS(fun, R_NilValue);
  }

  SEXP names = GET_NAMES(dots);

  SEXP args = R_NilValue;
  for (int i = n - 1; i >= 0; --i) {
    SEXP dot = VECTOR_ELT(dots, i);
    SEXP prom = lazy_to_promise(dot);
    args = PROTECT(CONS(prom, args));
    if (names != R_NilValue) {
      SEXP name = STRING_ELT(names, i);
      if (strlen(CHAR(name)) > 0)
        SET_TAG(args, Rf_installTrChar(name));
    }
  }
  UNPROTECT(n);

  SEXP call = LCONS(fun, args);

  return eval(call, env);
}
