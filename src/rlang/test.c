#include "rlang.h"

SEXP rlang_test_r_warn(SEXP x) {
  r_warn(CHAR(STRING_ELT(x, 0)));
  return R_NilValue;
}
