#include "rlang.h"


SEXP r_mut_env_parent(SEXP env, SEXP new_parent) {
  SET_ENCLOS(env, new_parent);
  return env;
}

bool r_is_env(SEXP x) {
  return r_typeof(x) == ENVSXP;
}

SEXP r_env_get(SEXP env, SEXP sym) {
  return Rf_findVarInFrame3(env, sym, TRUE);
}

SEXP r_base_env() {
  return R_BaseEnv;
}
