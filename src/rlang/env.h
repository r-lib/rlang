#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>
#include <Rversion.h>


#define r_global_env R_GlobalEnv
#define r_base_env R_BaseEnv
#define r_empty_env R_EmptyEnv

extern sexp* r_methods_ns_env;


static inline
sexp* r_env_names(sexp* env) {
  return R_lsInternal3(env, TRUE, FALSE);
}

static inline
r_ssize r_env_length(sexp* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("Expected an environment");
  }
  return Rf_xlength(env);
}

static inline
sexp* r_env_parent(sexp* env) {
  return ENCLOS(env);
}
static inline
void r_env_poke_parent(sexp* env, sexp* new_parent) {
  SET_ENCLOS(env, new_parent);
}

static inline
bool r_is_environment(sexp* x) {
  return TYPEOF(x) == ENVSXP;
}
static inline
bool r_is_namespace(sexp* x) {
  return R_IsNamespaceEnv(x);
}

static inline
sexp* r_env_find(sexp* env, sexp* sym) {
  return Rf_findVarInFrame3(env, sym, FALSE);
}
static inline
sexp* r_env_find_anywhere(sexp* env, sexp* sym) {
  return Rf_findVar(sym, env);
}

static inline
bool r_env_has(sexp* env, sexp* sym) {
  return r_env_find(env, sym) != r_syms.unbound;
}
static inline
bool r_env_has_anywhere(sexp* env, sexp* sym) {
  return r_env_find_anywhere(env, sym) != r_syms.unbound;
}

sexp* r_ns_env(const char* pkg);
sexp* r_base_ns_get(const char* name);

sexp* r_alloc_environment(r_ssize size, sexp* parent);

sexp* r_env_as_list(sexp* x);
sexp* r_list_as_environment(sexp* x, sexp* parent);
sexp* r_env_clone(sexp* env, sexp* parent);


static inline
void r_env_unbind(sexp* env, sexp* sym) {
#if (R_VERSION < R_Version(4, 0, 0))
  void r__env_unbind(sexp*, sexp*);
  r__env_unbind(env, sym);
#else
  R_removeVarFromFrame(sym, env);
#endif
}

static inline
sexp* r_env_poke(sexp* env, sexp* sym, sexp* value) {
  Rf_defineVar(sym, value, env);
  return env;
}
void r_env_poke_lazy(sexp* env, sexp* sym, sexp* expr, sexp* eval_env);

static inline
void r_env_poke_active(sexp* env, sexp* sym, sexp* fn) {
  if (r_env_has(env, sym)) {
    r_env_unbind(env, sym);
  }
  R_MakeActiveBinding(sym, fn, env);
}

bool r_env_inherits(sexp* env, sexp* ancestor, sexp* top);


#endif
