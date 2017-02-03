#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

bool is_call(SEXP x, const char* f) {
  if (!(Rf_isLanguage(x) || TYPEOF(x) == LISTSXP))
    return false;

  SEXP fun = CAR(x);
  if (!Rf_isSymbol(fun))
    return false;

  return fun == Rf_install(f);
}

bool is_lazy_load(SEXP x) {
  if (TYPEOF(x) != PROMSXP)
    return false;

  return is_call(PREXPR(x), "lazyLoadDBfetch");
}

SEXP last_cons(SEXP x) {
  while(CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}

SEXP length__(SEXP x) {
  return Rf_ScalarInteger(Rf_length(x));
}

int is_lang(SEXP x) {
  return TYPEOF(x) == LANGSXP || TYPEOF(x) == SYMSXP;
}

int is_true(SEXP x) {
  if (!IS_SCALAR(x, LGLSXP))
    Rf_error("`x` must be a boolean");

  int value = LOGICAL(x)[0];
  return value == NA_LOGICAL ? 0 : value;
}

// Formulas --------------------------------------------------------------------

bool is_formula(SEXP x) {
  if (TYPEOF(x) != LANGSXP)
    return 0;

  SEXP head = CAR(x);
  if (TYPEOF(head) != SYMSXP)
    return 0;

  const char* nm =  CHAR(PRINTNAME(head));
  return strcmp(nm, "~") == 0 || strcmp(nm, ":=") == 0;
}

bool is_fpromise(SEXP x) {
  return is_formula(x) && Rf_isNull(CDDR(x));
}

SEXP f_rhs_(SEXP f) {
  if (!is_formula(f))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  switch (Rf_length(f)) {
  case 2: return CADR(f);
  case 3: return CADDR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}

SEXP f_lhs_(SEXP f) {
  if (!is_formula(f))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  switch (Rf_length(f)) {
  case 2: return R_NilValue;
  case 3: return CADR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}

SEXP f_env_(SEXP f) {
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
