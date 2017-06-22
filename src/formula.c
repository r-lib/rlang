#include "rlang.h"


SEXP r_f_rhs(SEXP f) {
  if (TYPEOF(f) != LANGSXP)
    Rf_errorcall(R_NilValue, "`x` must be a formula");

  switch (r_length(f)) {
  case 2: return CADR(f);
  case 3: return CADDR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}
SEXP r_f_lhs(SEXP f) {
  if (TYPEOF(f) != LANGSXP)
    Rf_errorcall(R_NilValue, "`x` must be a formula");

  switch (r_length(f)) {
  case 2: return R_NilValue;
  case 3: return CADR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}
SEXP r_f_env(SEXP f) {
  return r_get_attr(f, r_sym(".Environment"));
}

bool r_f_has_env(SEXP f) {
  return r_is_env(r_f_env(f));
}

bool is_formulaish(SEXP x, int scoped, int lhs) {
  if (TYPEOF(x) != LANGSXP)
    return false;

  SEXP head = CAR(x);
  if (head != Rf_install("~") && head != Rf_install(":="))
    return false;

  if (scoped >= 0) {
    int has_env = TYPEOF(r_f_env(x)) == ENVSXP;
    if (scoped != has_env)
      return false;
  }

  if (lhs >= 0) {
    int has_lhs = r_length(x) > 2;
    if (lhs != has_lhs)
      return false;
  }

  return true;
}

bool is_formula(SEXP x) {
  if (!is_formulaish(x, -1, -1))
    return false;

  return CAR(x) == Rf_install("~");
}


// Export

int lgl_optional(SEXP lgl) {
  if (lgl == R_NilValue)
    return -1;
  else
    return r_as_bool(lgl);
}

SEXP rlang_is_formulaish(SEXP x, SEXP scoped, SEXP lhs) {
  int scoped_int = lgl_optional(scoped);
  int lhs_int = lgl_optional(lhs);

  bool out = is_formulaish(x, scoped_int, lhs_int);
  return Rf_ScalarLogical(out);
}
