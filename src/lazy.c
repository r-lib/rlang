#include <R.h>
#include <Rdefines.h>

SEXP promise_as_lazy(SEXP promise, SEXP env, int follow_symbols) {
  // recurse until we find the real promise, not a promise of a promise
  while(TYPEOF(promise) == PROMSXP) {
    env = PRENV(promise);
    promise = PREXPR(promise);

    // If the promise is threaded through multiple functions, we'll
    // get some symbols along the way. If the symbol is bound to a promise
    // keep going on up
    if (follow_symbols && TYPEOF(promise) == SYMSXP) {
      SEXP obj = findVar(promise, env);
      if (TYPEOF(obj) == PROMSXP) {
        promise = obj;
      }
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
