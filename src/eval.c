#define R_NO_REMAP
#include <Rinternals.h>

SEXP rlang_eval(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}
