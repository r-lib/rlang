#include "rlang.h"
#include "env.h"
#include "decl/env-binding-decl.h"

// https://bugs.r-project.org/show_bug.cgi?id=18928
#define RLANG_HAS_R_BINDING_API (R_VERSION >= R_Version(4, 6, 0))

#if !RLANG_HAS_R_BINDING_API
static inline r_obj* env_find(r_obj* env, r_obj* sym) {
  return Rf_findVarInFrame3(env, sym, FALSE);
}
#endif


static r_obj* new_binding_types(r_ssize n) {
  r_obj* types = r_alloc_integer(n);

  int* types_ptr = r_int_begin(types);
  r_memset(types_ptr, 0, n * sizeof *types_ptr);

  return types;
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
    enum r_env_binding_type type = r_env_binding_type(env, sym);
    if (type == R_ENV_BINDING_TYPE_active || type == R_ENV_BINDING_TYPE_delayed) {
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
  int* types_ptr = r_int_begin(types);

  // Fill value type for bindings before first special binding
  for (r_ssize j = 0; j < i; ++j) {
    *types_ptr = R_ENV_BINDING_TYPE_value;
    ++types_ptr;
  }

  while (i < n) {
    r_obj* sym = binding_as_sym(symbols, bindings, i);
    *types_ptr = r_env_binding_type(env, sym);

    ++i;
    ++types_ptr;
  }

  FREE(1);
  return types;
}

// R API: R_envSymbols
r_obj* r_env_syms(r_obj* env) {
#if RLANG_HAS_R_BINDING_API
  return R_envSymbols(env);
#else
  // This does an extra alloc, as does the initial implementation in https://github.com/r-devel/r-svn/commit/ee6dc5080845f911d7a884398213d22f3de63fe2
  r_obj* nms = KEEP(r_env_names(env));
  r_ssize n = r_length(nms);

  r_obj* out = KEEP(r_alloc_list(n));
  r_obj* const * v_nms = r_chr_cbegin(nms);

  for (r_ssize i = 0; i < n; ++i) {
    r_list_poke(out, i, r_str_as_symbol(v_nms[i]));
  }

  FREE(2);
  return out;
#endif
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

  r_obj* value = env_find(env, sym);

  if (value == R_UnboundValue) {
    return R_ENV_BINDING_TYPE_unbound;
  }

  if (value == r_missing_arg) {
    return R_ENV_BINDING_TYPE_missing;
  }

  if (r_typeof(value) == R_TYPE_promise) {
    bool forced;
    rlang_promise_unwrap(value, &forced);
    if (forced) {
      return R_ENV_BINDING_TYPE_forced;
    }

    return R_ENV_BINDING_TYPE_delayed;
  }

  return R_ENV_BINDING_TYPE_value;
#endif
}

r_obj* r_env_get(r_obj* env, r_obj* sym) {
  enum r_env_binding_type type = r_env_binding_type(env, sym);

  if (type == R_ENV_BINDING_TYPE_unbound) {
    r_abort("object '%s' not found", r_sym_c_string(sym));
  }
  if (type == R_ENV_BINDING_TYPE_missing) {
    return r_missing_arg;
  }

#if R_VERSION >= R_Version(4, 5, 0)
  return R_getVar(sym, env, FALSE);
#else
  r_obj* value = env_find(env, sym);
  if (r_typeof(value) == R_TYPE_dots) {
    return value;
  }

  // Handles value, delayed, forced, and active bindings
  return Rf_eval(sym, env);
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
  r_obj* promise = KEEP(Rf_allocSExp(PROMSXP));
  SET_PRCODE(promise, expr);
  SET_PRENV(promise, eval_env);
  SET_PRVALUE(promise, R_UnboundValue);
  Rf_defineVar(sym, promise, env);
  FREE(1);
#endif
}

void r_env_bind_forced(r_obj* env, r_obj* sym, r_obj* expr, r_obj* value) {
#if RLANG_HAS_R_BINDING_API
  R_MakeForcedBinding(sym, expr, value, env);
#else
  r_obj* promise = KEEP(Rf_allocSExp(PROMSXP));
  SET_PRCODE(promise, expr);
  SET_PRENV(promise, r_null);
  SET_PRVALUE(promise, value);
  Rf_defineVar(sym, promise, env);
  FREE(1);
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
  r_obj* value = env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("not a delayed binding");
  }

  bool forced;
  r_obj* inner = rlang_promise_unwrap(value, &forced);
  if (forced) {
    r_abort("not a delayed binding");
  }

  return R_PromiseExpr(inner);
#endif
}

r_obj* r_env_binding_delayed_env(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  return R_DelayedBindingEnvironment(sym, env);
#else
  r_obj* value = env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("not a delayed binding");
  }

  bool forced;
  r_obj* inner = rlang_promise_unwrap(value, &forced);
  if (forced) {
    r_abort("not a delayed binding");
  }

  return PRENV(inner);
#endif
}


// Forced binding accessors

r_obj* r_env_binding_forced_expr(r_obj* env, r_obj* sym) {
#if RLANG_HAS_R_BINDING_API
  return R_ForcedBindingExpression(sym, env);
#else
  r_obj* value = env_find(env, sym);

  if (r_typeof(value) != R_TYPE_promise) {
    r_abort("not a forced binding");
  }

  bool forced;
  r_obj* inner = rlang_promise_unwrap(value, &forced);
  if (!forced) {
    r_abort("not a forced binding");
  }

  return R_PromiseExpr(inner);
#endif
}

// Use `r_env_get()` to get the value of a forced binding


// Active binding accessors

r_obj* r_env_binding_active_fn(r_obj* env, r_obj* sym) {
  return R_ActiveBindingFunction(sym, env);
}
