#include <string.h>

#include "rlang.h"


int r_is_symbol(SEXP x, const char* string) {
  if (r_kind(x) != SYMSXP) {
    return false;
  } else {
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
  }
}

int r_is_symbol_any(SEXP x, const char** strings, int n) {
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
