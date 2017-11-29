#include "rlang/rlang.h"

sexp* rlang_test_r_warn(sexp* x) {
  r_warn(CHAR(STRING_ELT(x, 0)));
  return r_null;
}

sexp* rlang_r_string(sexp* str) {
  return STRING_ELT(str, 0);
}


// env.c

sexp* rlang_test_base_ns_get(sexp* name) {
  return r_base_ns_get(r_c_string(name));
}


// sym.c

sexp* rlang_test_is_special_op_sym(sexp* x) {
  return Rf_ScalarLogical(r_is_special_op_sym(x));
}


// squash.c

bool rlang_is_clevel_spliceable(sexp* x) {
  return Rf_inherits(x, "foo");
}
