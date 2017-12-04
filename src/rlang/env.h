#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>


#define r_base_env R_BaseEnv
#define r_empty_env R_EmptyEnv


static inline bool r_is_unbound_value(sexp* x) {
  return x == R_UnboundValue;
}

static inline void r_env_poke_parent(sexp* env, sexp* new_parent) {
  SET_ENCLOS(env, new_parent);
}

static inline bool r_is_environment(sexp* x) {
  return TYPEOF(x) == ENVSXP;
}

// TODO A more complete family that optionally looks up ancestry
// The `find` variant does not fail if object does not exist
static inline sexp* r_env_find(sexp* env, sexp* sym) {
  return Rf_findVarInFrame3(env, sym, TRUE);
}
static inline sexp* r_env_get(sexp* env, sexp* sym) {
  return Rf_eval(sym, env);
}
static inline sexp* r_env_poke(sexp* env, sexp* sym, sexp* value) {
  Rf_defineVar(sym, value, env);
  return env;
}

static inline sexp* r_env_parent(sexp* env) {
  return ENCLOS(env);
}

sexp* r_ns_env(const char* pkg);

sexp* r_base_ns_get(const char* name);

sexp* r_new_environment(sexp* parent);


#endif
