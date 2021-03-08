#include <string.h>

#include "rlang.h"


// In old R versions `as.name()` does not translate to native which
// loses the encoding. This symbol constructor always translates.
sexp* r_new_symbol(sexp* x, int* err) {
  switch (r_typeof(x)) {
  case SYMSXP:
    return x;
  case R_TYPE_character:
    if (r_length(x) == 1) {
      const char* string = Rf_translateChar(r_chr_get(x, 0));
      return r_sym(string);
    } // else fallthrough
  default: {
    if (err) {
      *err = -1;
      return r_null;
    } else {
      const char* type = r_type_as_c_string(r_typeof(x));
      r_abort("Can't create a symbol with a %s", type);
    }
  }}
}

bool r_is_symbol(sexp* x, const char* string) {
  if (r_typeof(x) != SYMSXP) {
    return false;
  } else {
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
  }
}

bool r_is_symbol_any(sexp* x, const char** strings, int n) {
  if (r_typeof(x) != SYMSXP) {
    return false;
  }

  const char* name = CHAR(PRINTNAME(x));

  for (int i = 0; i < n; ++i) {
    if (strcmp(name, strings[i]) == 0) {
      return true;
    }
  }

  return false;
}
