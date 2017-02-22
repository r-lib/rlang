#define R_NO_REMAP
#include <Rinternals.h>
#include <stdbool.h>

extern Rboolean latin1locale;
extern Rboolean utf8locale;

SEXP rlang_symbol(SEXP chr) {

  int real_utf8locale = utf8locale;
  utf8locale = true;

  SEXP string = STRING_ELT(chr, 0);
  SEXP symbol = Rf_installChar(string);

  utf8locale = real_utf8locale;
  return symbol;
}
