#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>
#include <Rversion.h>


#define r_global_env R_GlobalEnv
#define r_base_env R_BaseEnv
#define r_empty_env R_EmptyEnv

#if (!defined(R_VERSION) || R_VERSION < R_Version(3, 2, 0))
static inline sexp* r_env_names(sexp* env) {
  return R_lsInternal(env, true);
}
#else
static inline sexp* r_env_names(sexp* env) {
  return R_lsInternal3(env, true, false);
}
#endif

static inline sexp* r_env_parent(sexp* env) {
  return ENCLOS(env);
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

sexp* r_ns_env(const char* pkg);
sexp* r_base_ns_get(const char* name);

sexp* r_new_environment(sexp* parent, r_ssize_t size);

sexp* r_env_as_list(sexp* x);
sexp* r_list_as_environment(sexp* x, sexp* parent);
sexp* r_env_clone(sexp* env, sexp* parent);

sexp* r_env_unbind_names(sexp* env, sexp* names, bool inherits);
sexp* r_env_unbind_all(sexp* env, const char** names, r_ssize_t n, bool inherits);
sexp* r_env_unbind(sexp* env, const char* name, bool inherits);


#endif
