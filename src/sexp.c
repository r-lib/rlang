#define R_NO_REMAP
#include <Rinternals.h>

SEXP rlang_sxp_address(SEXP x) {
  static char str[1000];
  snprintf(str, 1000, "%p", (void*) x);
  return Rf_mkString(str);
}

SEXP rlang_is_reference(SEXP x, SEXP y) {
  return Rf_ScalarLogical(x == y);
}
