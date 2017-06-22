#include "rlang.h"

bool r_as_bool(SEXP x) {
  if (TYPEOF(x) != LGLSXP && r_length(x) != 1)
    r_abort("Expected a scalar logical");
  int* xp = (int*) LOGICAL(x);
  return *xp;
}
