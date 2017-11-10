#include "rlang/rlang.h"


// expr-node.c

SEXP rlang_node_car(SEXP x) {
  return CAR(x);
}
SEXP rlang_node_cdr(SEXP x) {
  return CDR(x);
}
SEXP rlang_node_caar(SEXP x) {
  return CAAR(x);
}
SEXP rlang_node_cadr(SEXP x) {
  return CADR(x);
}
SEXP rlang_node_cdar(SEXP x) {
  return CDAR(x);
}
SEXP rlang_node_cddr(SEXP x) {
  return CDDR(x);
}
SEXP rlang_node_tail(SEXP x) {
  while (CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}

SEXP rlang_node_poke_car(SEXP x, SEXP newcar) {
  SETCAR(x, newcar);
  return x;
}
SEXP rlang_node_poke_cdr(SEXP x, SEXP newcdr) {
  SETCDR(x, newcdr);
  return x;
}
SEXP rlang_node_poke_caar(SEXP x, SEXP newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
SEXP rlang_node_poke_cadr(SEXP x, SEXP newcar) {
  SETCADR(x, newcar);
  return x;
}
SEXP rlang_node_poke_cdar(SEXP x, SEXP newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
SEXP rlang_node_poke_cddr(SEXP x, SEXP newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

SEXP rlang_new_node_(SEXP car, SEXP cdr) {
  return Rf_cons(car, cdr);
}

SEXP rlang_node_tag(SEXP x) {
  return TAG(x);
}
SEXP rlang_node_poke_tag(SEXP x, SEXP tag) {
  SET_TAG(x, tag);
  return x;
}

SEXP rlang_on_exit(SEXP expr, SEXP frame) {
  r_on_exit(expr, frame);
  return r_null;
}
