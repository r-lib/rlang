#define R_NO_REMAP
#include <Rinternals.h>

// These change attributes in-place.

SEXP rlang_zap_attrs(SEXP x) {
  SET_ATTRIB(x, R_NilValue);
  return x;
}

SEXP rlang_set_attrs(SEXP x, SEXP attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}

SEXP rlang_get_attrs(SEXP x) {
  return ATTRIB(x);
}
