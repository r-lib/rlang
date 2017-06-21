#define R_NO_REMAP
#include <Rinternals.h>

SEXP r_new_language_(SEXP head, SEXP tail) {
  return Rf_lcons(head, tail);
}
SEXP r_new_language(SEXP head, SEXP tail) {
  PROTECT(head);
  PROTECT(tail);
  SEXP out = Rf_lcons(head, tail);
  UNPROTECT(2);
  return out;
}
