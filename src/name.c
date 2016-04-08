#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "utils.h"

SEXP lhs_name(SEXP x) {
  if (TYPEOF(x) != VECSXP)
    Rf_errorcall(R_NilValue, "`x` must be a list (not a %s)", Rf_type2char(TYPEOF(x)));

  int n = Rf_length(x);
  SEXP x2 = Rf_shallow_duplicate(x);

  SEXP names = Rf_getAttrib(x2, R_NamesSymbol);
  if (names == R_NilValue) {
    names = Rf_allocVector(STRSXP, n);
    Rf_setAttrib(x2, R_NamesSymbol, names);
  }

  for (int i = 0; i < n; ++i) {
    SEXP xi = VECTOR_ELT(x2, i);
    if (!is_formula(xi) || Rf_length(xi) != 3)
      continue;

    SEXP name = Rf_eval(lhs(xi), f_env(xi));
    if (TYPEOF(name) != STRSXP || Rf_length(name) != 1)
      Rf_errorcall(R_NilValue, "LHS must evaluate to a single string");

    SET_VECTOR_ELT(x2, i, Rf_lang2(CAR(xi), CADDR(xi)));
    SET_STRING_ELT(names, i, STRING_ELT(name, 0));
  }

  return x2;
}
