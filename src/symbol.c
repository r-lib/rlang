#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

SEXP rlang_symbol(SEXP chr) {
  SEXP string = STRING_ELT(chr, 0);
  return Rf_install(Rf_translateChar(string));
}
