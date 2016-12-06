#include <R.h>
#include <Rdefines.h>
#include "utils.h"

SEXP inspect_dot(SEXP dot, SEXP env) {

  while (TYPEOF(dot) == PROMSXP) {
    env = PRENV(dot);
    if (env == R_NilValue)
      Rf_error("Promise has already been forced");

    dot = PREXPR(dot);
  }

  if (NAMED(dot) < 2)
    SET_NAMED(dot, 2);
  SEXP dot_info = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(dot_info, 0, dot);
  SET_VECTOR_ELT(dot_info, 1, env);

  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("expr"));
  SET_STRING_ELT(names, 1, mkChar("env"));
  setAttrib(dot_info, install("names"), names);

  UNPROTECT(2);
  return dot_info;
}

SEXP inspect_dots_(SEXP env) {
  SEXP dots = findVar(R_DotsSymbol, env);
  int n = Rf_length(dots);

  if (dots == R_MissingArg) {
    return Rf_allocVector(VECSXP, 0);
  }

  SEXP dots_info = PROTECT(allocVector(VECSXP, n));
  SEXP names = PROTECT(allocVector(STRSXP, n));

  int i = 0;
  for(SEXP nxt = dots; nxt != R_NilValue; nxt = CDR(nxt)) {
    SEXP dot = CAR(nxt);

    SET_VECTOR_ELT(dots_info, i, inspect_dot(dot, env));

    if (TAG(nxt) != R_NilValue)
      SET_STRING_ELT(names, i, PRINTNAME(TAG(nxt)));

    i++;
  }
  setAttrib(dots_info, install("names"), names);

  UNPROTECT(2);
  return dots_info;
}
