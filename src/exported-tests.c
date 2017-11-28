#include "rlang/rlang.h"

SEXP rlang_test_r_warn(SEXP x) {
  r_warn(CHAR(STRING_ELT(x, 0)));
  return r_null;
}

SEXP rlang_r_string(SEXP str) {
  return STRING_ELT(str, 0);
}


// env.c

SEXP rlang_test_base_ns_get(SEXP name) {
  return r_base_ns_get(r_c_string(name));
}


// sym.c

SEXP rlang_test_is_special_op_sym(SEXP x) {
  return Rf_ScalarLogical(r_is_special_op_sym(x));
}


// squash.c

bool rlang_is_clevel_spliceable(SEXP x) {
  return Rf_inherits(x, "foo");
}
