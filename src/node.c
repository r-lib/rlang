#include <Rinternals.h>

SEXP r_node_car(SEXP x) {
  return CAR(x);
}
SEXP r_node_cdr(SEXP x) {
  return CDR(x);
}
SEXP r_node_caar(SEXP x) {
  return CAAR(x);
}
SEXP r_node_cadr(SEXP x) {
  return CADR(x);
}
SEXP r_node_cdar(SEXP x) {
  return CDAR(x);
}
SEXP r_node_cddr(SEXP x) {
  return CDDR(x);
}

SEXP r_mut_node_car(SEXP x, SEXP newcar) {
  SETCAR(x, newcar);
  return x;
}
SEXP r_mut_node_cdr(SEXP x, SEXP newcdr) {
  SETCDR(x, newcdr);
  return x;
}
SEXP r_mut_node_caar(SEXP x, SEXP newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
SEXP r_mut_node_cadr(SEXP x, SEXP newcar) {
  SETCADR(x, newcar);
  return x;
}
SEXP r_mut_node_cdar(SEXP x, SEXP newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
SEXP r_mut_node_cddr(SEXP x, SEXP newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

SEXP r_cons(SEXP car, SEXP cdr) {
  return CONS(car, cdr);
}

SEXP r_duplicate(SEXP x) {
  return Rf_duplicate(x);
}
SEXP r_shallow_duplicate(SEXP x) {
  return Rf_shallow_duplicate(x);
}

SEXP r_node_tag(SEXP x) {
  return TAG(x);
}
SEXP r_mut_node_tag(SEXP x, SEXP tag) {
  SET_TAG(x, tag);
  return x;
}
