#include "rlang.h"

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

// The underscored version expects protected arguments
SEXP r_new_node_(SEXP car, SEXP cdr) {
  return Rf_cons(car, cdr);
}
SEXP r_new_node(SEXP car, SEXP cdr) {
  PROTECT(car);
  PROTECT(cdr);
  SEXP out = Rf_cons(car, cdr);
  UNPROTECT(2);
  return out;
}

SEXP r_new_pairlist(SEXP car) {
  return r_new_node(car, R_NilValue);
}
SEXP r_new_pairlist2(SEXP car1, SEXP car2) {
  return r_new_node(car1, r_new_pairlist(car2));
}
SEXP r_new_pairlist3(SEXP car1, SEXP car2, SEXP car3) {
  return r_new_node(car1, r_new_pairlist2(car2, car3));
}

SEXP r_node_tag(SEXP x) {
  return TAG(x);
}
SEXP r_mut_node_tag(SEXP x, SEXP tag) {
  SET_TAG(x, tag);
  return x;
}
