#include "rlang.h"

SEXP r_new_node(SEXP car, SEXP cdr) {
  PROTECT(car);
  PROTECT(cdr);
  SEXP out = Rf_cons(car, cdr);
  UNPROTECT(2);
  return out;
}

SEXP r_new_pairlist(SEXP car) {
  return r_new_node(car, r_null);
}
SEXP r_new_pairlist2(SEXP car1, SEXP car2) {
  return r_new_node(car1, r_new_pairlist(car2));
}
SEXP r_new_pairlist3(SEXP car1, SEXP car2, SEXP car3) {
  return r_new_node(car1, r_new_pairlist2(car2, car3));
}
