#include <Rinternals.h>

SEXP rlang_car(SEXP x) {
  return CAR(x);
}
SEXP rlang_cdr(SEXP x) {
  return CDR(x);
}
SEXP rlang_cadr(SEXP x) {
  return CADR(x);
}
SEXP rlang_cddr(SEXP x) {
  return CDDR(x);
}

SEXP rlang_set_car(SEXP x, SEXP newcar) {
  SETCAR(x, newcar);
  return x;
}
SEXP rlang_set_cdr(SEXP x, SEXP newcdr) {
  SETCDR(x, newcdr);
  return x;
}
SEXP rlang_set_cadr(SEXP x, SEXP newcar) {
  SETCADR(x, newcar);
  return x;
}
SEXP rlang_set_cddr(SEXP x, SEXP newcdr) {
  SEXP cdr = CDR(x);
  SETCDR(cdr, newcdr);
  return x;
}

SEXP rlang_cons(SEXP car, SEXP cdr) {
  return CONS(car, cdr);
}

SEXP rlang_duplicate(SEXP x) {
  return Rf_duplicate(x);
}

SEXP rlang_tag(SEXP x) {
  return TAG(x);
}
SEXP rlang_set_tag(SEXP x, SEXP tag) {
  SET_TAG(x, tag);
  return x;
}
