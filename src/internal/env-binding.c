#include <rlang.h>
#include "internal.h"
#include "env.h"
#include "quo.h"

#include "decl/env-decl.h"


r_obj* ffi_env_get(r_obj* env,
                   r_obj* nm,
                   r_obj* inherit,
                   r_obj* last,
                   r_obj* closure_env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment.");
  }
  if (!r_is_string(nm)) {
    r_abort("`nm` must be a string.");
  }
  if (!r_is_bool(inherit)) {
    r_abort("`inherit` must be a logical value.");
  }

  bool c_inherit = r_lgl_get(inherit, 0);

  r_obj* sym = r_str_as_symbol(r_chr_get(nm, 0));
  return env_get_sym(env, sym, c_inherit, last, closure_env);
}

// This util is a little more complex than it would be by calling `getVar()`
// directly because it evaluates `default` lazily
static
r_obj* env_get_sym(r_obj* env,
                   r_obj* sym,
                   bool inherit,
                   r_obj* last,
                   r_obj* closure_env) {
  if (r_typeof(last) != R_TYPE_environment) {
    r_abort("`last` must be an environment.");
  }

  bool unbound;
  if (inherit) {
    if (last == r_null) {
      unbound = !r_env_has_anywhere(env, sym);
    } else {
      unbound = !r_env_has_until(env, sym, last);
    }
  } else {
    unbound = !r_env_has(env, sym);
  }

  if (unbound) {
    // Can't use `r_env_get()` here because we have a custom error
    // when `default` is missing
    if (r_env_find(closure_env, r_sym("default")) == r_missing_arg) {
      struct r_pair args[] = {
        { r_sym("nm"), KEEP(r_str_as_character(r_sym_string(sym))) }
      };
      r_exec_n(r_null,
               r_sym("stop_env_get_missing"),
               args,
               R_ARR_SIZEOF(args),
               closure_env);
      r_stop_unreachable();
    }

    return r_eval(r_sym("default"), closure_env);
  }

  if (inherit) {
    if (last == r_null) {
      return r_env_get_anywhere(env, sym);
    } else {
      return r_env_get_until(env, sym, last);
    }
  } else {
    return r_env_get(env, sym);
  }
}

r_obj* ffi_env_get_list(r_obj* env,
                        r_obj* nms,
                        r_obj* inherit,
                        r_obj* last,
                        r_obj* closure_env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment.");
  }
  if (r_typeof(nms) != R_TYPE_character) {
    r_abort("`nm` must be a string.");
  }
  if (!r_is_bool(inherit)) {
    r_abort("`inherit` must be a logical value.");
  }

  bool c_inherit = r_lgl_get(inherit, 0);
  r_ssize n = r_length(nms);

  r_obj* out = KEEP(r_alloc_list(n));
  r_attrib_poke_names(out, nms);

  r_obj* const * p_nms = r_chr_cbegin(nms);

  for (r_ssize i = 0; i <n; ++i) {
    r_obj* sym = r_str_as_symbol(p_nms[i]);
    r_obj* elt = env_get_sym(env, sym, c_inherit, last, closure_env);
    r_list_poke(out, i, elt);
  }

  FREE(1);
  return out;
}

r_obj* ffi_env_has(r_obj* env, r_obj* nms, r_obj* inherit) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment.");
  }
  if (r_typeof(nms) != R_TYPE_character) {
    r_abort("`nms` must be a character vector.");
  }
  if (r_typeof(inherit) != R_TYPE_logical) {
    r_abort("`inherit` must be a logical value.");
  }

  r_ssize n = r_length(nms);
  r_obj* out = KEEP(r_alloc_logical(n));

  int* p_out = r_lgl_begin(out);
  r_obj* const * p_nms = r_chr_cbegin(nms);

  if (r_lgl_get(inherit, 0)) {
    for (r_ssize i = 0; i < n; ++i) {
      r_obj* sym = r_str_as_symbol(p_nms[i]);
      p_out[i] = r_env_has_anywhere(env, sym);
    }
  } else {
    for (r_ssize i = 0; i < n; ++i) {
      r_obj* sym = r_str_as_symbol(p_nms[i]);
      p_out[i] = r_env_has(env, sym);
    }
  }

  r_attrib_poke_names(out, nms);
  FREE(1);
  return out;
}

static void env_poke_or_zap(r_obj* env, r_obj* sym, r_obj* value);
static void env_poke_lazy(r_obj* env, r_obj* sym, r_obj* value, r_obj* eval_env);
static void env_poke_active(r_obj* env, r_obj* sym, r_obj* fn, r_obj* eval_env);
static r_obj* env_get(r_obj* env, r_obj* sym);

r_obj* ffi_env_poke(r_obj* env, r_obj* nm, r_obj* value, r_obj* ffi_inherit, r_obj* ffi_create) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment.");
  }
  if (!r_is_string(nm)) {
    r_abort("`nm` must be a string.");
  }
  if (!r_is_bool(ffi_inherit)) {
    r_abort("`inherit` must be a logical value.");
  }
  if (!r_is_bool(ffi_create)) {
    r_abort("`create` must be a logical value.");
  }

  bool inherit = r_lgl_get(ffi_inherit, 0);
  bool create = r_lgl_get(ffi_create, 0);
  r_obj* sym = r_str_as_symbol(r_chr_get(nm, 0));

  bool unbound;
  if (inherit) {
    unbound = !r_env_has_anywhere(env, sym);
  } else {
    unbound = !r_env_has(env, sym);
  }

  r_obj* old;
  if (unbound) {
    if (!create) {
      r_abort("Can't find existing binding in `env` for \"%s\".",
              r_sym_c_string(sym));
    }
    old = rlang_zap;
  } else if (inherit) {
    old = r_env_get_anywhere(env, sym);
  } else {
    old = r_env_get(env, sym);
  }
  KEEP(old);

  if (inherit && !unbound) {
    while (env != r_envs.empty) {
      if (r_env_has(env, sym)) {
        break;
      }
      env = r_env_parent(env);
    }
  }
  env_poke_or_zap(env, sym, value);

  FREE(1);
  return old;
}


enum bind_type {
  BIND_TYPE_value,
  BIND_TYPE_active,
  BIND_TYPE_lazy
};

enum bind_type parse_bind_type(r_obj* bind_type) {
  switch (*r_chr_get_c_string(bind_type, 0)) {
  case 'v': return BIND_TYPE_value;
  case 'a': return BIND_TYPE_active;
  case 'l': return BIND_TYPE_lazy;
  default: r_stop_unreachable();
  }
}

r_obj* ffi_env_bind(r_obj* env,
                      r_obj* values,
                      r_obj* needs_old,
                      r_obj* bind_type,
                      r_obj* eval_env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment.");
  }

  bool c_needs_old = r_lgl_get(needs_old, 0);
  enum bind_type c_bind_type = parse_bind_type(bind_type);

  if (r_typeof(values) != R_TYPE_list) {
    r_stop_internal("`values` must be a list.");
  }

  r_ssize n = r_length(values);
  if (!n) {
    return r_globals.empty_list;
  }

  r_obj* names = r_names(values);
  if (n && names == r_null) {
    r_abort("Can't bind data because some elements are not named.");
  }
  r_obj* const * p_names = r_chr_cbegin(names);

  r_obj* old = r_null;
  if (c_needs_old) {
    old = KEEP(r_alloc_list(n));
    r_attrib_poke_names(old, names);
  } else {
    KEEP(old);
  }

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* sym = r_str_as_symbol(p_names[i]);
    r_obj* value = r_list_get(values, i);

    if (c_needs_old) {
      r_list_poke(old, i, env_get(env, sym));
    }

    if (value == rlang_zap) {
      r_env_unbind(env, sym);
    } else {
      switch (c_bind_type) {
      case BIND_TYPE_value: r_env_poke(env, sym, value); break;
      case BIND_TYPE_lazy: env_poke_lazy(env, sym, value, eval_env); break;
      case BIND_TYPE_active: env_poke_active(env, sym, value, eval_env); break;
      }
    }
  }

  FREE(1);
  return old;
}

r_obj* ffi_env_unbind(r_obj* env, r_obj* names, r_obj* inherits) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment.");
  }
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector.");
  }
  if (!r_is_bool(inherits)) {
    r_abort("`inherits` must be a logical value.");
  }

  if (*r_lgl_begin(inherits)) {
    r_env_unbind_anywhere_names(env, names);
  } else {
    r_env_unbind_names(env, names);
  }

  return r_null;
}


static
void env_poke_or_zap(r_obj* env, r_obj* sym, r_obj* value) {
  if (value == rlang_zap) {
    r_env_unbind(env, sym);
  } else {
    r_env_poke(env, sym, value);
  }
}
static
void env_poke_lazy(r_obj* env, r_obj* sym, r_obj* expr, r_obj* eval_env) {
  if (is_quosure(expr)) {
    expr = KEEP(rlang_as_function(expr, eval_env));
    expr = r_new_call(expr, r_null);
    FREE(1);
  }
  KEEP(expr);

  r_env_poke_lazy(env, sym, expr, eval_env);
  FREE(1);
}
static
void env_poke_active(r_obj* env, r_obj* sym, r_obj* fn, r_obj* eval_env) {
  if (!r_is_function(fn)) {
    fn = rlang_as_function(fn, eval_env);
  }
  KEEP(fn);

  r_env_poke_active(env, sym, fn);
  FREE(1);
}

static
r_obj* env_get(r_obj* env, r_obj* sym) {
  // Can't use `r_env_get()` because we can't error with missing arguments
  r_obj* out = r_env_find(env, sym);

  if (out == r_syms.unbound) {
    return rlang_zap;
  }

  if (r_typeof(out) == R_TYPE_promise) {
    KEEP(out);
    out = r_eval(out, r_envs.base);
    FREE(1);
  }

  return out;
}
