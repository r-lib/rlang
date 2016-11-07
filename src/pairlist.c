#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP car_(SEXP x) {
  return CAR(x);
}
SEXP cdr_(SEXP x) {
  return CDR(x);
}
SEXP cadr_(SEXP x) {
  return CADR(x);
}
SEXP cddr_(SEXP x) {
  return CDDR(x);
}
SEXP set_car_(SEXP x, SEXP newcar) {
  SETCAR(x, newcar);
  return x;
}
SEXP set_cdr_(SEXP x, SEXP newcdr) {
  SETCDR(x, newcdr);
  return x;
}
SEXP set_cadr_(SEXP x, SEXP newcar) {
  SETCADR(x, newcar);
  return x;
}
SEXP set_cddr_(SEXP x, SEXP newcdr) {
  SEXP cdr = CDR(x);
  SETCDR(cdr, newcdr);
  return x;
}
