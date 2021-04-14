#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>
#include <Rversion.h>


#define r_global_env R_GlobalEnv
#define r_base_env R_BaseEnv
#define r_empty_env R_EmptyEnv

extern r_obj* r_methods_ns_env;


static inline
r_obj* r_env_names(r_obj* env) {
  return R_lsInternal3(env, TRUE, FALSE);
}

static inline
r_ssize r_env_length(r_obj* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("Expected an environment");
  }
  return Rf_xlength(env);
}

static inline
r_obj* r_env_parent(r_obj* env) {
  return ENCLOS(env);
}
static inline
void r_env_poke_parent(r_obj* env, r_obj* new_parent) {
  SET_ENCLOS(env, new_parent);
}

static inline
bool r_is_environment(r_obj* x) {
  return TYPEOF(x) == ENVSXP;
}
static inline
bool r_is_namespace(r_obj* x) {
  return R_IsNamespaceEnv(x);
}

static inline
r_obj* r_env_find(r_obj* env, r_obj* sym) {
  return Rf_findVarInFrame3(env, sym, FALSE);
}
static inline
r_obj* r_env_find_anywhere(r_obj* env, r_obj* sym) {
  return Rf_findVar(sym, env);
}

static inline
bool r_env_has(r_obj* env, r_obj* sym) {
  return r_env_find(env, sym) != r_syms.unbound;
}
static inline
bool r_env_has_anywhere(r_obj* env, r_obj* sym) {
  return r_env_find_anywhere(env, sym) != r_syms.unbound;
}

r_obj* r_ns_env(const char* pkg);
r_obj* r_base_ns_get(const char* name);

r_obj* r_alloc_environment(r_ssize size, r_obj* parent);

r_obj* r_env_as_list(r_obj* x);
r_obj* r_list_as_environment(r_obj* x, r_obj* parent);
r_obj* r_env_clone(r_obj* env, r_obj* parent);


static inline
void r_env_unbind(r_obj* env, r_obj* sym) {
#if (R_VERSION < R_Version(4, 0, 0))
  void r__env_unbind(r_obj*, r_obj*);
  r__env_unbind(env, sym);
#else
  R_removeVarFromFrame(sym, env);
#endif
}

static inline
r_obj* r_env_poke(r_obj* env, r_obj* sym, r_obj* value) {
  Rf_defineVar(sym, value, env);
  return env;
}
void r_env_poke_lazy(r_obj* env, r_obj* sym, r_obj* expr, r_obj* eval_env);

static inline
void r_env_poke_active(r_obj* env, r_obj* sym, r_obj* fn) {
  if (r_env_has(env, sym)) {
    r_env_unbind(env, sym);
  }
  R_MakeActiveBinding(sym, fn, env);
}

bool r_env_inherits(r_obj* env, r_obj* ancestor, r_obj* top);


#endif
