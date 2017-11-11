#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>


#define r_base_env R_BaseEnv
#define r_empty_env R_EmptyEnv


static inline bool r_is_unbound_value(SEXP x) {
  return x == R_UnboundValue;
}

static inline void r_env_poke_parent(SEXP env, SEXP new_parent) {
  SET_ENCLOS(env, new_parent);
}

static inline bool r_is_environment(SEXP x) {
  return TYPEOF(x) == ENVSXP;
}

static inline SEXP r_env_get(SEXP env, SEXP sym) {
  return Rf_findVarInFrame3(env, sym, TRUE);
}
static inline SEXP r_env_set(SEXP env, SEXP sym, SEXP value) {
  Rf_defineVar(sym, value, env);
  return env;
}

SEXP r_ns_env(const char* pkg);


#endif
