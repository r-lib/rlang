#include <string.h>

#include "rlang.h"


// In old R versions `as.name()` does not translate to native which
// loses the encoding. This symbol constructor always translates.
SEXP r_new_symbol(SEXP x, int* err) {
  switch (r_kind(x)) {
  case SYMSXP:
    return x;
  case STRSXP:
    if (r_length(x) == 1) {
      const char* string = Rf_translateChar(r_chr_get(x, 0));
      return r_sym(string);
    } // else fallthrough
  default: {
    if (err) {
      *err = -1;
      return r_null;
    } else {
      const char* type = r_type_c_string(r_kind(x));
      r_abort("Can't create a symbol with a %s", type);
    }
  }}
}

bool r_is_symbol(SEXP x, const char* string) {
  if (r_kind(x) != SYMSXP) {
    return false;
  } else {
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
  }
}

bool r_is_symbol_any(SEXP x, const char** strings, int n) {
  if (r_kind(x) != SYMSXP) {
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

bool r_is_special_op_sym(SEXP x) {
  if (r_kind(x) != SYMSXP) {
    return false;
  }

  const char* name = CHAR(PRINTNAME(x));
  size_t len = strlen(name);

  return
    len > 2 &&
    name[0] == '%' &&
    name[len - 1] == '%';
}
