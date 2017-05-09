#define R_NO_REMAP
#include <Rinternals.h>
#include <stdbool.h>


SEXP f_rhs_(SEXP f) {
  if (TYPEOF(f) != LANGSXP)
    Rf_errorcall(R_NilValue, "`x` must be a formula");

  switch (Rf_length(f)) {
  case 2: return CADR(f);
  case 3: return CADDR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}
SEXP f_lhs_(SEXP f) {
  if (TYPEOF(f) != LANGSXP)
    Rf_errorcall(R_NilValue, "`x` must be a formula");

  switch (Rf_length(f)) {
  case 2: return R_NilValue;
  case 3: return CADR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}
SEXP f_env_(SEXP f) {
  return Rf_getAttrib(f, Rf_install(".Environment"));
}

bool is_formulaish(SEXP x, int scoped, int lhs) {
  if (TYPEOF(x) != LANGSXP)
    return false;

  SEXP head = CAR(x);
  if (head != Rf_install("~") && head != Rf_install(":="))
    return false;

  if (scoped >= 0) {
    int has_env = TYPEOF(f_env_(x)) == ENVSXP;
    if (scoped != has_env)
      return false;
  }

  if (lhs >= 0) {
    int has_lhs = Rf_length(x) > 2;
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
    return Rf_asLogical(lgl);
}

SEXP rlang_is_formulaish(SEXP x, SEXP scoped, SEXP lhs) {
  int scoped_int = lgl_optional(scoped);
  int lhs_int = lgl_optional(lhs);

  bool out = is_formulaish(x, scoped_int, lhs_int);
  return Rf_ScalarLogical(out);
}
