#include "rlang.h"
#include "env.h"


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
  memset(types_ptr, 0, n * sizeof *types_ptr);

  return types;
}

static enum r_env_binding_type which_env_binding(r_obj* env, r_obj* sym) {
  if (r_env_binding_is_active(env, sym)) {
    // Check for active bindings first, since promise detection triggers
    // active bindings through `r_env_find()` (#1376)
    return R_ENV_BINDING_TYPE_active;
  }

  if (r_env_binding_is_promise(env, sym)) {
    return R_ENV_BINDING_TYPE_promise;
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
