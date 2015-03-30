#include <R.h>
#include <Rdefines.h>

int is_dead_end(SEXP x) {
  if (TYPEOF(PREXPR(x)) == LANGSXP) {
    const char* name = CHAR(PRINTNAME(CAR(PREXPR(x))));
    if (strcmp(name, "lazyLoadDBfetch") == 0) {
      return 1;
    }
  }
  return 0;
}

SEXP promise_as_lazy(SEXP promise, SEXP env, int follow_symbols) {
  // recurse until we find the real promise, not a promise of a promise
  // stop when we find a lazily loaded object
  int dead_end = 0;

  while(TYPEOF(promise) == PROMSXP && !dead_end) {
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

      dead_end = is_dead_end(obj);
      if (!dead_end && TYPEOF(obj) == PROMSXP) {
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

SEXP make_lazy_dots(SEXP env, SEXP follow_symbols_) {
  SEXP dots = findVar(install("..."), env);
  int follow_symbols = asLogical(follow_symbols_);

  // Figure out how many elements in dots
  int n = 0;
  for(SEXP nxt = dots; nxt != R_NilValue; nxt = CDR(nxt)) {
    n++;
  }

  // Allocate list to store results
  SEXP lazy_dots = PROTECT(allocVector(VECSXP, n));
  SEXP names = PROTECT(allocVector(STRSXP, n));

  // Iterate through all elements of dots, converting promises into lazy exprs
  int i = 0;
  SEXP nxt = dots;
  while(nxt != R_NilValue) {
    SEXP promise = CAR(nxt);

    SEXP lazy = promise_as_lazy(promise, env, follow_symbols);
    SET_VECTOR_ELT(lazy_dots, i, lazy);
    if (TAG(nxt) != R_NilValue)
      SET_STRING_ELT(names, i, PRINTNAME(TAG(nxt)));

    nxt = CDR(nxt);
    i++;
  }
  setAttrib(lazy_dots, install("names"), names);
  setAttrib(lazy_dots, install("class"), PROTECT(mkString("lazy_dots")));

  UNPROTECT(3);

  return lazy_dots;
}
