// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ENV_BINDING_H
#define RLANG_ENV_BINDING_H

#include "rlang-types.h"

enum r_env_binding_type {
  R_ENV_BINDING_TYPE_unbound = 0,
  R_ENV_BINDING_TYPE_value = 1,
  R_ENV_BINDING_TYPE_missing = 2,
  R_ENV_BINDING_TYPE_delayed = 3,
  R_ENV_BINDING_TYPE_forced = 4,
  R_ENV_BINDING_TYPE_active = 5
};

enum r_env_binding_type r_env_binding_type(r_obj* env, r_obj* sym);

bool r_env_binding_is_promise(r_obj* env, r_obj* sym);
bool r_env_binding_is_active(r_obj* env, r_obj* sym);
r_obj* r_env_binding_types(r_obj* env, r_obj* bindings);

// Binding constructors
static inline
void r_env_bind(r_obj* env, r_obj* sym, r_obj* value) {
  KEEP(value);
  Rf_defineVar(sym, value, env);
  FREE(1);
}

static inline
void r_env_poke(r_obj* env, r_obj* sym, r_obj* value) {
  r_env_bind(env, sym, value);
}

void r_env_bind_active(r_obj* env, r_obj* sym, r_obj* fn);
void r_env_bind_delayed(r_obj* env, r_obj* sym, r_obj* expr, r_obj* eval_env);
void r_env_bind_forced(r_obj* env, r_obj* sym, r_obj* expr, r_obj* value);
void r_env_bind_missing(r_obj* env, r_obj* sym);

// Delayed binding accessors
r_obj* r_env_binding_delayed_expr(r_obj* env, r_obj* sym);
r_obj* r_env_binding_delayed_env(r_obj* env, r_obj* sym);

// Forced binding accessors
r_obj* r_env_binding_forced_expr(r_obj* env, r_obj* sym);
r_obj* r_env_binding_forced_value(r_obj* env, r_obj* sym);

// Active binding accessors
r_obj* r_env_binding_active_fn(r_obj* env, r_obj* sym);

void r_init_library_env_binding(void);


#endif
