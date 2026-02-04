#include "rlang.h"
#include "env.h"
#include "decl/env-binding-decl.h"

// https://bugs.r-project.org/show_bug.cgi?id=18928
#define RLANG_HAS_R_BINDING_API 0


bool r_env_binding_is_promise(r_obj* env, r_obj* sym) {
  r_obj* obj = r_env_find(env, sym);
  return r_typeof(obj) == R_TYPE_promise && PRVALUE(obj) == r_syms.unbound;
}
bool r_env_binding_is_active(r_obj* env, r_obj* sym) {
  return R_BindingIsActive(sym, env);
}

static r_obj* new_binding_types(r_ssize n) {
  r_obj* types = r_alloc_integer(n);

  int* types_ptr = r_int_begin(types);
  r_memset(types_ptr, 0, n * sizeof *types_ptr);

  return types;
}

static enum r_env_binding_type which_env_binding(r_obj* env, r_obj* sym) {
  if (r_env_binding_is_active(env, sym)) {
    // Check for active bindings first, since promise detection triggers
    // active bindings through `r_env_find()` (#1376)
    return R_ENV_BINDING_TYPE_active;
  }

  if (r_env_binding_is_promise(env, sym)) {
    return R_ENV_BINDING_TYPE_delayed;
  }

  return R_ENV_BINDING_TYPE_value;
}

static inline r_obj* binding_as_sym(bool list, r_obj* bindings, r_ssize i) {
  if (list) {
    r_obj* out = r_list_get(bindings, i);

    if (r_typeof(out) != R_TYPE_symbol) {
      r_abort("Binding must be a symbol.");
    }

    return out;
  } else {
    return r_str_as_symbol(r_chr_get(bindings, i));
  }
}

static r_ssize detect_special_binding(r_obj* env,
                                      r_obj* bindings,
                                      bool symbols) {
  r_ssize n = r_length(bindings);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* sym = binding_as_sym(symbols, bindings, i);
    if (which_env_binding(env, sym)) {
      return i;
    }
  }

  return -1;
}

// Returns NULL if all values to spare an alloc
r_obj* r_env_binding_types(r_obj* env, r_obj* bindings) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("Expected environment in promise binding predicate.");
  }

  bool symbols;
  switch (r_typeof(bindings)) {
  case R_TYPE_list: symbols = true; break;
  case R_TYPE_character: symbols = false; break;
  default: r_abort("Internal error: Unexpected `bindings` type in `r_env_binding_types()`");
  }

  r_ssize i = detect_special_binding(env, bindings, symbols);
  if (i < 0) {
    return r_null;
  }

  r_ssize n = r_length(bindings);
  r_obj* types = KEEP(new_binding_types(n));
  int* types_ptr = r_int_begin(types) + i;

  while (i < n) {
    r_obj* sym = binding_as_sym(symbols, bindings, i);
    *types_ptr = which_env_binding(env, sym);

    ++i;
    ++types_ptr;
  }

  FREE(1);
  return types;
}


// Binding type API
// Implements future R API from https://bugs.r-project.org/show_bug.cgi?id=18928

enum r_env_binding_type r_env_binding_type(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  switch (R_GetBindingType(sym, env)) {
  case R_BindingTypeUnbound: return R_ENV_BINDING_TYPE_unbound;
  case R_BindingTypeValue:   return R_ENV_BINDING_TYPE_value;
  case R_BindingTypeMissing: return R_ENV_BINDING_TYPE_missing;
  case R_BindingTypeDelayed: return R_ENV_BINDING_TYPE_delayed;
  case R_BindingTypeForced:  return R_ENV_BINDING_TYPE_forced;
  case R_BindingTypeActive:  return R_ENV_BINDING_TYPE_active;
  }
  r_stop_unreachable();
#else
  // Active binding check must come first since `r_env_find()` triggers them
  if (R_BindingIsActive(sym, env)) {
    return R_ENV_BINDING_TYPE_active;
  }

  r_obj* value = r_env_find(env, sym);

  if (value == r_syms.unbound) {
    return R_ENV_BINDING_TYPE_unbound;
  }

  if (value == r_missing_arg) {
    return R_ENV_BINDING_TYPE_missing;
  }

  if (r_typeof(value) == R_TYPE_promise) {
    if (PRVALUE(value) == r_syms.unbound) {
      return R_ENV_BINDING_TYPE_delayed;
    } else {
      return R_ENV_BINDING_TYPE_forced;
    }
  }

  return R_ENV_BINDING_TYPE_value;
#endif
}


// Binding constructors

void r_env_bind_active(r_obj* env, r_obj* sym, r_obj* fn) {
  KEEP(fn);
  r_env_unbind(env, sym);
  R_MakeActiveBinding(sym, fn, env);
  FREE(1);
}

void r_env_bind_delayed(r_obj* env, r_obj* sym, r_obj* expr, r_obj* eval_env) {
#if RLANG_HAS_R_BINDING_API
  R_MakeDelayedBinding(sym, expr, eval_env, env);
#else
  KEEP(expr);
  r_obj* name = KEEP(r_sym_as_utf8_character(sym));

  r_node_poke_car(bind_delayed_value_node, expr);
  r_eval_with_xyz(bind_delayed_call, name, env, eval_env, rlang_ns_env);
  r_node_poke_car(bind_delayed_value_node, r_null);

  FREE(2);
#endif
}

void r_env_bind_forced(r_obj* env, r_obj* sym, r_obj* expr, r_obj* value) {
#if RLANG_HAS_R_BINDING_API
  R_MakeForcedBinding(sym, expr, value, env);
#else
  // Creating an evaluated promise requires internal R API (`R_mkEVPROMISE`).
  // Create a delayed binding and force it manually.
  r_env_bind_delayed(env, sym, expr, r_envs.empty);

  r_obj* promise = r_env_find(env, sym);
  SET_PRVALUE(promise, value);
  SET_PRENV(promise, r_null);
#endif
}

void r_env_bind_missing(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  R_MakeMissingBinding(sym, env);
#else
  Rf_defineVar(sym, r_missing_arg, env);
#endif
}


// Delayed binding accessors

r_obj* r_env_binding_delayed_expr(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  return R_DelayedBindingExpression(sym, env);
#else
  r_obj* value = r_env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("Not a promise binding.");
  }
  if (PRVALUE(value) != r_syms.unbound) {
    r_abort("Not a delayed binding.");
  }

  return R_PromiseExpr(value);
#endif
}

r_obj* r_env_binding_delayed_env(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  return R_DelayedBindingEnvironment(sym, env);
#else
  r_obj* value = r_env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("Not a promise binding.");
  }
  if (PRVALUE(value) != r_syms.unbound) {
    r_abort("Not a delayed binding.");
  }

  return PRENV(value);
#endif
}


// Forced binding accessors

r_obj* r_env_binding_forced_expr(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  return R_ForcedBindingExpression(sym, env);
#else
  r_obj* value = r_env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("Not a promise binding.");
  }
  if (PRVALUE(value) == r_syms.unbound) {
    r_abort("Not a forced binding.");
  }

  return R_PromiseExpr(value);
#endif
}

r_obj* r_env_binding_forced_value(r_obj* env, r_obj* sym) {
  r_obj* value = r_env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("Not a promise binding.");
  }
  if (PRVALUE(value) == r_syms.unbound) {
    r_abort("Not a forced binding.");
  }

  return PRVALUE(value);
}


// Active binding accessors

r_obj* r_env_binding_active_fn(r_obj* env, r_obj* sym) {
  return R_ActiveBindingFunction(sym, env);
}


void r_init_library_env_binding(void) {
  bind_delayed_call = r_parse("delayedAssign(x, value = NULL, assign.env = y, eval.env = z)");
  r_preserve(bind_delayed_call);

  bind_delayed_value_node = r_node_cddr(bind_delayed_call);
}
