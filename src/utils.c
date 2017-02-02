#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

int is_sym(SEXP x, const char* string) {
  if (TYPEOF(x) != SYMSXP)
    return false;
  else
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
}

int is_lang(SEXP x) {
  return TYPEOF(x) == LANGSXP || TYPEOF(x) == SYMSXP;
}

bool is_call(SEXP x, const char* f) {
  if (!is_lang(x) && TYPEOF(x) != LISTSXP)
    return false;

  SEXP fun = CAR(x);
  return is_sym(fun, f);
}

int is_prefixed_call(SEXP x, const char* fn) {
  if (TYPEOF(x) != LANGSXP)
    return 0;

  SEXP head = CAR(x);
  if (!(is_call(head, "$") ||
        is_call(head, "@") ||
        is_call(head, "::") ||
        is_call(head, ":::")))
    return 0;

  if (fn == NULL)
    return 1;

  SEXP args = CDAR(x);
  SEXP sym = CADR(args);
  return is_sym(sym, fn);
}

SEXP last_cons(SEXP x) {
  while(CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}

SEXP length__(SEXP x) {
  return Rf_ScalarInteger(Rf_length(x));
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

  return is_sym(head, "~") || is_sym(head, ":=");
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

SEXP rlang_fun(SEXP sym) {
  SEXP prefixed_sym = PROTECT(Rf_lang3(Rf_install("::"), Rf_install("rlang"), sym));
  SEXP fun = Rf_eval(prefixed_sym, R_BaseEnv);
  UNPROTECT(1);
  return fun;
}
