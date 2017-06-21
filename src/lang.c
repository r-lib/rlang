#include "rlang.h"

SEXP r_new_language_(SEXP head, SEXP tail) {
  return Rf_lcons(head, tail);
}
SEXP r_new_language(SEXP head, SEXP tail) {
  KEEP(head);
  KEEP(tail);
  SEXP out = Rf_lcons(head, tail);
  FREE(2);
  return out;
}

bool r_is_lang(SEXP x) {
  return TYPEOF(x) == LANGSXP;
}
