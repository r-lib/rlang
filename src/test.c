#include "rlang/rlang.h"

SEXP rlang_test_r_warn(SEXP x) {
  r_warn(CHAR(STRING_ELT(x, 0)));
  return R_NilValue;
}

SEXP rlang_r_string(SEXP str) {
  return STRING_ELT(str, 0);
}


// squash.c

bool rlang_is_clevel_spliceable(SEXP x) {
  return Rf_inherits(x, "foo");
}
