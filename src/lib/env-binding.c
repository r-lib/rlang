#include "rlang.h"


bool r_env_binding_is_promise(sexp* env, sexp* sym) {
  if (r_typeof(sym) != r_type_symbol) {
    r_abort("Internal error: Expected symbol in promise binding predicate");
  }

  sexp* obj = r_env_find(env, sym);
  return r_typeof(obj) == r_type_promise && PRVALUE(obj) == r_unbound_sym;
}
bool r_env_binding_is_active(sexp* env, sexp* sym) {
  if (r_typeof(sym) != r_type_symbol) {
    r_abort("Internal error: Expected symbol in active binding predicate");
  }
  return R_BindingIsActive(sym, env);
}

static sexp* new_binding_types(r_ssize n) {
  sexp* types = r_new_vector(r_type_integer, n);

  int* types_ptr = r_int_deref(types);
  memset(types_ptr, 0, n * sizeof *types_ptr);

  return types;
}

static enum r_env_binding_type which_env_binding(sexp* env, sexp* sym) {
  if (r_env_binding_is_promise(env, sym)) {
    return R_ENV_BINDING_PROMISE;
  }

  if (r_env_binding_is_active(env, sym)) {
    return R_ENV_BINDING_ACTIVE;
  }

  return R_ENV_BINDING_VALUE;
}

// Returns NULL if all values to spare an alloc
sexp* r_env_binding_types(sexp* env, sexp* bindings) {
  bool all_values = true;

  bool symbols;
  switch (r_typeof(bindings)) {
  case r_type_list: symbols = true; break;
  case r_type_character: symbols = false; break;
  default: r_abort("Internal error: Unexpected `bindings` type in `r_env_binding_types()`");
  }

  int n_protect = 0;

  r_ssize n = r_length(bindings);
  sexp* types = NULL;
  int* types_ptr = NULL;

  for (r_ssize i = 0; i < n; ++i) {
    sexp* sym;
    if (symbols) {
      sym = r_list_get(bindings, i);
    } else {
      sym = r_str_as_symbol(r_chr_get(bindings, i));
    }

    enum r_env_binding_type type = which_env_binding(env, sym);
    if (type != R_ENV_BINDING_VALUE) {
      all_values = false;
    }

    if (type) {
      if (!types) {
        types = KEEP_N(new_binding_types(n), n_protect);
        types_ptr = r_int_deref(types);
      }
      types_ptr[i] = type;
    }
  }

  FREE(n_protect);
  if (all_values) {
    return r_null;
  } else {
    return types;
  }
}
