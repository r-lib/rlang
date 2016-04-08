#include <R.h>
#include <Rdefines.h>
#include "utils.h"

SEXP promise_as_lazy(SEXP promise, SEXP env, int follow_symbols) {
  // recurse until we find the real promise, not a promise of a promise
  while(TYPEOF(promise) == PROMSXP) {
    if (PRENV(promise) == R_NilValue) {
      Rf_error("Promise has already been forced");
    }

    env = PRENV(promise);
    promise = PREXPR(promise);

    // If the promise is threaded through multiple functions, we'll
    // get some symbols along the way. If the symbol is bound to a promise
    // keep going on up
    if (follow_symbols && TYPEOF(promise) == SYMSXP) {
      SEXP obj = findVar(promise, env);

      if (TYPEOF(obj) != PROMSXP)
        break;

      if (is_lazy_load(obj))
        break;

      promise = obj;
    }
  }

  // Make named list for output
  SEXP lazy = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(lazy, 0, promise);
  SET_VECTOR_ELT(lazy, 1, env);

  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("expr"));
  SET_STRING_ELT(names, 1, mkChar("env"));

  setAttrib(lazy, install("names"), names);
  setAttrib(lazy, install("class"), PROTECT(mkString("lazy")));

  UNPROTECT(3);

  return lazy;
}

SEXP make_lazy(SEXP name, SEXP env, SEXP follow_symbols_) {
  SEXP promise = findVar(name, env);
  int follow_symbols = asLogical(follow_symbols_);

  return promise_as_lazy(promise, env, follow_symbols);
}

int is_missing(SEXP x) {
  return TYPEOF(x) == SYMSXP && x == R_MissingArg;
}

SEXP make_lazy_dots(SEXP env, SEXP follow_symbols_, SEXP ignore_empty_) {
  SEXP dots = findVar(R_DotsSymbol, env);
  int follow_symbols = asLogical(follow_symbols_);
  int ignore_empty = asLogical(ignore_empty_);

  if (dots == R_MissingArg) {
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 0));
    setAttrib(out, install("class"), PROTECT(mkString("lazy_dots")));
    UNPROTECT(2);
    return out;
  }

  // Figure out how many elements in dots
  int n = 0;
  for(SEXP nxt = dots; nxt != R_NilValue; nxt = CDR(nxt)) {
    if (ignore_empty && is_missing(CAR(nxt)))
      continue;

    n++;
  }

  // Allocate list to store results
  SEXP lazy_dots = PROTECT(allocVector(VECSXP, n));
  SEXP names = PROTECT(allocVector(STRSXP, n));

  // Iterate through all elements of dots, converting promises into lazy exprs
  int i = 0;
  for(SEXP nxt = dots; nxt != R_NilValue; nxt = CDR(nxt)) {
    SEXP promise = CAR(nxt);

    if (ignore_empty && is_missing(promise))
      continue;

    SEXP lazy = promise_as_lazy(promise, env, follow_symbols);
    SET_VECTOR_ELT(lazy_dots, i, lazy);
    if (TAG(nxt) != R_NilValue)
      SET_STRING_ELT(names, i, PRINTNAME(TAG(nxt)));

    i++;
  }
  setAttrib(lazy_dots, install("names"), names);
  setAttrib(lazy_dots, install("class"), PROTECT(mkString("lazy_dots")));

  UNPROTECT(3);

  return lazy_dots;
}
