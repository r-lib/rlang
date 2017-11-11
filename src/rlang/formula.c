#include "rlang.h"


SEXP r_f_rhs(SEXP f) {
  if (r_kind(f) != LANGSXP) {
    r_abort("`x` must be a formula");
  }

  switch (r_length(f)) {
  case 2: return r_node_cadr(f);
  case 3: return CADDR(f);
  default: r_abort("Invalid formula");
  }
}
SEXP r_f_lhs(SEXP f) {
  if (r_kind(f) != LANGSXP) {
    r_abort("`x` must be a formula");
  }

  switch (r_length(f)) {
  case 2: return r_null;
  case 3: return r_node_cadr(f);
  default: r_abort("Invalid formula");
  }
}
SEXP r_f_env(SEXP f) {
  return r_get_attribute(f, r_sym(".Environment"));
}

bool r_f_has_env(SEXP f) {
  return r_is_environment(r_f_env(f));
}

bool r_is_formulaish(SEXP x, int scoped, int lhs) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  SEXP head = r_node_car(x);
  if (head != r_sym("~") && head != r_sym(":=")) {
    return false;
  }

  if (scoped >= 0) {
    int has_env = r_kind(r_f_env(x)) == ENVSXP;
    if (scoped != has_env) {
      return false;
    }
  }

  if (lhs >= 0) {
    int has_lhs = r_length(x) > 2;
    if (lhs != has_lhs) {
      return false;
    }
  }

  return true;
}
