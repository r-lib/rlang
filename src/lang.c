#define R_NO_REMAP
#include <Rinternals.h>

SEXP r_new_language(SEXP head, SEXP tail) {
  return Rf_lcons(head, tail);
}
