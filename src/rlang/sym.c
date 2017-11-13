#include <string.h>

#include "rlang.h"


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
