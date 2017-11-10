#include <string.h>

#include "rlang.h"

int r_is_symbol(SEXP x, const char* string) {
  if (r_kind(x) != SYMSXP)
    return false;
  else
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
}
