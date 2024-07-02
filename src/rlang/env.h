// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include "rlang-types.h"
#include "cnd.h"
#include "globals.h"
#include "obj.h"


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
  if (env == r_envs.empty) {
    r_stop_internal("Can't take the parent of the empty environment.");
  }
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

#if R_VERSION < R_Version(4, 5, 0)
static inline
r_obj* r_env_get(r_obj* env, r_obj* sym) {
  r_obj* out = r_env_find(env, sym);

  if (out == r_syms.unbound) {
    r_abort("object '%s' not found", r_sym_c_string(sym));
  }
  if (r_typeof(out) == R_TYPE_promise) {
    Rf_eval(out, env);
  }

  return out;
}

static inline
r_obj* r_env_get_anywhere(r_obj* env, r_obj* sym) {
  r_obj* out = r_env_find_anywhere(env, sym);

  if (out == r_syms.unbound) {
    r_abort("object '%s' not found", r_sym_c_string(sym));
  }

  return out;
}
#else
static inline
r_obj* r_env_get(r_obj* env, r_obj* sym) {
  return R_getVar(sym, env, FALSE);
}

static inline
r_obj* r_env_get_anywhere(r_obj* env, r_obj* sym) {
  return R_getVar(sym, env, TRUE);
}
#endif


static inline
r_obj* r_env_find(r_obj* env, r_obj* sym) {
  return Rf_findVarInFrame3(env, sym, FALSE);
}
static inline
r_obj* r_env_find_anywhere(r_obj* env, r_obj* sym) {
  return Rf_findVar(sym, env);
}
r_obj* r_env_find_until(r_obj* env, r_obj* sym, r_obj* last);


// TODO: Enable `R_existsVarInFrame()` when R 4.2 is out
#define RLANG_USE_R_EXISTS (1 || R_VERSION < R_Version(4, 2, 0))

static inline
bool r_env_has(r_obj* env, r_obj* sym) {
#if RLANG_USE_R_EXISTS
  bool r__env_has(r_obj*, r_obj*);
  return r__env_has(env, sym);
#else
  return R_existsVarInFrame(env, sym);
#endif
}

static inline
bool r_env_has_anywhere(r_obj* env, r_obj* sym) {
#if RLANG_USE_R_EXISTS
  bool r__env_has_anywhere(r_obj*, r_obj*);
  return r__env_has_anywhere(env, sym);
#else
  return TODO();
#endif
}

r_obj* r_ns_env(const char* pkg);
r_obj* r_base_ns_get(const char* name);

r_obj* r_alloc_environment(r_ssize size, r_obj* parent);

static inline
r_obj* r_alloc_empty_environment(r_obj* parent) {
  // Non-hashed environment.
  // Very fast and useful when you aren't getting/setting from the result.
  r_obj* env = Rf_allocSExp(R_TYPE_environment);
  r_env_poke_parent(env, parent);
  return env;
}

r_obj* r_env_as_list(r_obj* x);
r_obj* r_list_as_environment(r_obj* x, r_obj* parent);
r_obj* r_env_clone(r_obj* env, r_obj* parent);

void r_env_coalesce(r_obj* env, r_obj* from);


// Silently ignores bindings that are not defined in `env`.
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
void r_env_poke(r_obj* env, r_obj* sym, r_obj* value) {
  KEEP(value);
  Rf_defineVar(sym, value, env);
  FREE(1);
}

void r_env_poke_lazy(r_obj* env, r_obj* sym, r_obj* expr, r_obj* eval_env);

static inline
void r_env_poke_active(r_obj* env, r_obj* sym, r_obj* fn) {
  KEEP(fn);
  r_env_unbind(env, sym);
  R_MakeActiveBinding(sym, fn, env);
  FREE(1);
}

bool r_env_inherits(r_obj* env, r_obj* ancestor, r_obj* top);


#endif
