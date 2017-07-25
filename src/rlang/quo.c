#include "rlang.h"

bool r_quo_is_missing(SEXP x) {
  return r_is_missing(r_f_rhs(x));
}
