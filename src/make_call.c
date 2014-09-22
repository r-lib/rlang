#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>

// This is a bit naughty, but there's no other way to create a promise
SEXP Rf_mkPROMISE(SEXP, SEXP);

SEXP make_call_(SEXP fun, SEXP dots) {
  if (TYPEOF(fun) != SYMSXP && TYPEOF(fun) != LANGSXP) {
    error("fun must be a call or a symbol");
  }
  if (TYPEOF(dots) != VECSXP) {
    error("dots must be a list");
  }
  if (!inherits(dots, "lazy_dots")) {
    error("dots must be of class lazy_dots");
  }

  int n = length(dots);
  SEXP names = GET_NAMES(dots);

  SEXP args = R_NilValue;
  for (int i = n - 1; i >= 0; --i) {
    // arg is a list of length 2 - LANGSXP/SYMSXP, followed by ENVSXP
    SEXP dot = VECTOR_ELT(dots, i);
    SEXP prom = PROTECT(Rf_mkPROMISE(VECTOR_ELT(dot, 0), VECTOR_ELT(dot, 1)));
    args = PROTECT(CONS(prom, args));
    UNPROTECT(1);
    SET_TAG(args, Rf_install(CHAR(STRING_ELT(names, i))));
  }
  UNPROTECT(n);

  return LCONS(fun, args);
}
