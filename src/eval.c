#include "rlang.h"

SEXP r_eval(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}
