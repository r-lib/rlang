#include <rlang.h>
#include "internal.h"


static sexp* rlang_env_get_sym(sexp* env, sexp* nm, bool inherit, sexp* closure_env);

sexp* rlang_env_get(sexp* env, sexp* nm, sexp* inherit, sexp* closure_env) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  if (!r_is_string(nm, NULL)) {
    r_abort("`nm` must be a string.");
  }
  if (!r_is_bool(inherit)) {
    r_abort("`inherit` must be a logical value.");
  }

  bool c_inherit = r_lgl_get(inherit, 0);

  sexp* sym = r_str_as_symbol(r_chr_get(nm, 0));
  return rlang_env_get_sym(env, sym, c_inherit, closure_env);
}

static
sexp* rlang_env_get_sym(sexp* env, sexp* sym, bool inherit, sexp* closure_env) {
  sexp* out;
  if (inherit) {
    out = r_env_find_anywhere(env, sym);
  } else {
    out = r_env_find(env, sym);
  }

  if (r_typeof(out) == r_type_promise) {
    KEEP(out);
    out = r_eval(out, r_empty_env);
    FREE(1);
  }

  if (out == r_syms_unbound) {
    out = r_eval(r_sym("default"), closure_env);
  }

  return out;
}

sexp* rlang_env_get_list(sexp* env, sexp* nms, sexp* inherit, sexp* closure_env) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  if (r_typeof(nms) != r_type_character) {
    r_abort("`nm` must be a string.");
  }
  if (!r_is_bool(inherit)) {
    r_abort("`inherit` must be a logical value.");
  }

  bool c_inherit = r_lgl_get(inherit, 0);
  r_ssize n = r_length(nms);

  sexp* out = KEEP(r_new_vector(r_type_list, n));
  r_poke_names(out, nms);

  sexp* const * p_nms = r_chr_deref_const(nms);

  for (r_ssize i = 0; i <n; ++i) {
    sexp* sym = r_str_as_symbol(p_nms[i]);
    sexp* elt = rlang_env_get_sym(env, sym, c_inherit, closure_env);
    r_list_poke(out, i, elt);
  }

  FREE(1);
  return out;
}

sexp* rlang_env_has(sexp* env, sexp* nms, sexp* inherit) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  if (r_typeof(nms) != r_type_character) {
    r_abort("`nms` must be a character vector.");
  }
  if (r_typeof(inherit) != r_type_logical) {
    r_abort("`inherit` must be a logical value.");
  }

  r_ssize n = r_length(nms);
  sexp* out = KEEP(r_new_vector(r_type_logical, n));

  int* p_out = r_lgl_deref(out);
  sexp* const * p_nms = r_chr_deref_const(nms);

  if (r_lgl_get(inherit, 0)) {
    for (r_ssize i = 0; i < n; ++i) {
      sexp* sym = r_str_as_symbol(p_nms[i]);
      p_out[i] = r_env_has_anywhere(env, sym);
    }
  } else {
    for (r_ssize i = 0; i < n; ++i) {
      sexp* sym = r_str_as_symbol(p_nms[i]);
      p_out[i] = r_env_has(env, sym);
    }
  }

  r_poke_names(out, nms);
  FREE(1);
  return out;
}

static void env_poke_or_zap(sexp* env, sexp* sym, sexp* value);
static void env_poke_lazy(sexp* env, sexp* sym, sexp* value, sexp* eval_env);
static void env_poke_active(sexp* env, sexp* sym, sexp* fn, sexp* eval_env);
static sexp* env_get(sexp* env, sexp* sym);

sexp* rlang_env_poke(sexp* env, sexp* nm, sexp* value, sexp* inherit, sexp* create) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  if (!r_is_string(nm, NULL)) {
    r_abort("`nm` must be a string.");
  }
  if (!r_is_bool(inherit)) {
    r_abort("`inherit` must be a logical value.");
  }
  if (!r_is_bool(create)) {
    r_abort("`create` must be a logical value.");
  }

  bool c_inherit = r_lgl_get(inherit, 0);
  bool c_create = r_lgl_get(create, 0);
  sexp* sym = r_str_as_symbol(r_chr_get(nm, 0));

  sexp* old;
  if (c_inherit) {
    old = r_env_find_anywhere(env, sym);
  } else {
    old = r_env_find(env, sym);
  }

  bool absent = (old == r_syms_unbound);
  if (absent) {
    if (!c_create) {
      r_abort("Can't find existing binding in `env` for \"%s\".",
              r_sym_get_c_string(sym));
    }
    old = rlang_zap;
  }
  KEEP(old);

  if (c_inherit && !absent) {
    while (env != r_empty_env) {
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

enum bind_type parse_bind_type(sexp* bind_type) {
  switch (*r_chr_get_c_string(bind_type, 0)) {
  case 'v': return BIND_TYPE_value;
  case 'a': return BIND_TYPE_active;
  case 'l': return BIND_TYPE_lazy;
  default: never_reached("parse_bind_type");
  }
}

sexp* rlang_env_bind(sexp* env,
                     sexp* values,
                     sexp* needs_old,
                     sexp* bind_type,
                     sexp* eval_env) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }

  bool c_needs_old = r_lgl_get(needs_old, 0);
  enum bind_type c_bind_type = parse_bind_type(bind_type);

  if (r_typeof(values) != r_type_list) {
    r_stop_internal("rlang_env_bind", "`values` must be a list.");
  }

  r_ssize n = r_length(values);
  if (!n) {
    return r_shared_empty_list;
  }

  sexp* names = r_names(values);
  if (n && names == r_null) {
    r_abort("Can't bind data because some elements are not named.");
  }
  sexp* const * p_names = r_chr_deref_const(names);

  sexp* old = r_null;
  if (c_needs_old) {
    old = KEEP(r_new_vector(r_type_list, n));
    r_poke_names(old, names);
  } else {
    KEEP(old);
  }

  for (r_ssize i = 0; i < n; ++i) {
    sexp* sym = r_str_as_symbol(p_names[i]);
    sexp* value = r_list_get(values, i);

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

sexp* rlang_env_unbind(sexp* env, sexp* names, sexp* inherits) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  if (r_typeof(names) != r_type_character) {
    r_abort("`names` must be a character vector.");
  }
  if (!r_is_bool(inherits)) {
    r_abort("`inherits` must be a logical value.");
  }

  if (*r_lgl_deref(inherits)) {
    r_env_unbind_anywhere_names(env, names);
  } else {
    r_env_unbind_names(env, names);
  }

  return r_null;
}


static
void env_poke_or_zap(sexp* env, sexp* sym, sexp* value) {
  if (value == rlang_zap) {
    r_env_unbind(env, sym);
  } else {
    r_env_poke(env, sym, value);
  }
}
static
void env_poke_lazy(sexp* env, sexp* sym, sexp* expr, sexp* eval_env) {
  if (rlang_is_quosure(expr)) {
    expr = KEEP(r_as_function(expr, eval_env));
    expr = r_new_call(expr, r_null);
    FREE(1);
  }
  KEEP(expr);

  r_env_poke_lazy(env, sym, expr, eval_env);
  FREE(1);
}
static
void env_poke_active(sexp* env, sexp* sym, sexp* fn, sexp* eval_env) {
  if (!r_is_function(fn)) {
    fn = r_as_function(fn, eval_env);
  }
  KEEP(fn);

  r_env_poke_active(env, sym, fn);
  FREE(1);
}

static
sexp* env_get(sexp* env, sexp* sym) {
  sexp* out = r_env_find(env, sym);

  if (out == r_syms_unbound) {
    return rlang_zap;
  }

  if (r_typeof(out) == r_type_promise) {
    KEEP(out);
    out = r_eval(out, r_base_env);
    FREE(1);
  }

  return out;
}
