#include "rlang.h"

SEXP rlang_sxp_address(SEXP x) {
  static char str[1000];
  snprintf(str, 1000, "%p", (void*) x);
  return Rf_mkString(str);
}

SEXP rlang_is_reference(SEXP x, SEXP y) {
  return r_scalar_lgl(x == y);
}

SEXPTYPE r_typeof(SEXP x) {
  return TYPEOF(x);
}

bool r_inherits(SEXP x, const char* class_) {
  return Rf_inherits(x, class_);
}

SEXP r_get_attr(SEXP x, SEXP sym) {
  return Rf_getAttrib(x, sym);
}

void mut_attr(SEXP x, SEXP sym, SEXP attr) {
  Rf_setAttrib(x, sym, attr);
}
void mut_class(SEXP x, SEXP classes) {
  Rf_setAttrib(x, R_ClassSymbol, classes);
}

SEXP set_attr(SEXP x, SEXP sym, SEXP attr) {
  x = KEEP(Rf_shallow_duplicate(x));
  mut_attr(x, sym, attr);

  FREE(1);
  return x;
}
SEXP set_class(SEXP x, SEXP classes) {
  return set_attr(x, R_ClassSymbol, classes);
}

SEXP sxp_class(SEXP x) {
  return Rf_getAttrib(x, R_ClassSymbol);
}
SEXP sxp_names(SEXP x) {
  return Rf_getAttrib(x, R_NamesSymbol);
}

void mut_names(SEXP x, SEXP nms) {
  Rf_setAttrib(x, R_NamesSymbol, nms);
}

bool is_named(SEXP x) {
  SEXP nms = sxp_names(x);

  if (TYPEOF(nms) != STRSXP)
    return false;

  if (chr_has(nms, ""))
    return false;

  return true;
}


SEXP r_missing_arg() {
  return R_MissingArg;
}
bool r_is_missing(SEXP x) {
  return x == R_MissingArg;
}


SEXP rlang_is_null(SEXP x) {
  return r_scalar_lgl(r_is_null(x));
}
bool r_is_null(SEXP x) {
  return x == R_NilValue;
}

SEXP r_duplicate(SEXP x, bool shallow) {
  if (shallow)
    return Rf_shallow_duplicate(x);
  else
    return Rf_duplicate(x);
}
SEXP rlang_duplicate(SEXP x, SEXP shallow) {
  return r_duplicate(x, r_as_bool(shallow));
}
