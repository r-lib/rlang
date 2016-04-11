#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

bool is_scalar(SEXP x) {
  return Rf_isVectorAtomic(x) && Rf_length(x) == 1;
}

bool is_call_to(SEXP x, const char* f) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP fun = CAR(x);
  if (!Rf_isSymbol(fun))
    return false;

  return fun == Rf_install(f);
}

bool is_lazy_load(SEXP x) {
  if (TYPEOF(x) != PROMSXP)
    return false;

  return is_call_to(PREXPR(x), "lazyLoadDBfetch");
}

SEXP findLast(SEXP x) {
  SEXP cons = x;
  while(CDR(cons) != R_NilValue)
    cons = CDR(cons);

  return cons;
}

// Formulas --------------------------------------------------------------------

bool is_formula(SEXP x) {
  return TYPEOF(x) == LANGSXP && Rf_inherits(x, "formula");
}

SEXP rhs(SEXP f) {
  if (!is_formula(f))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  switch (Rf_length(f)) {
  case 2: return CADR(f);
  case 3: return CADDR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}

SEXP lhs(SEXP f) {
  if (!is_formula(f))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  switch (Rf_length(f)) {
  case 2: return R_NilValue;
  case 3: return CADR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}

SEXP f_env(SEXP f) {
  if (!is_formula(f))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  return Rf_getAttrib(f, Rf_install(".Environment"));
}

SEXP make_formula1(SEXP rhs, SEXP env) {
  SEXP f = PROTECT(Rf_lang2(Rf_install("~"), rhs));
  Rf_setAttrib(f, R_ClassSymbol, Rf_mkString("formula"));
  Rf_setAttrib(f, Rf_install(".Environment"), env);

  UNPROTECT(1);
  return f;
}
