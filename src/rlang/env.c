#include "rlang.h"

#include "decl/env-decl.h"

r_obj* eval_with_x(r_obj* call, r_obj* x);
r_obj* eval_with_xy(r_obj* call, r_obj* x, r_obj* y);
r_obj* eval_with_xyz(r_obj* call, r_obj* x, r_obj* y, r_obj* z);


r_obj* r_ns_env(const char* pkg) {
  r_obj* ns = r_env_find(R_NamespaceRegistry, r_sym(pkg));
  if (ns == r_syms.unbound) {
    r_abort("Can't find namespace `%s`", pkg);
  }
  return ns;
}

static
r_obj* ns_env_get(r_obj* env, const char* name) {
  r_obj* obj = KEEP(r_env_find(env, r_sym(name)));

  // Can be a promise to a lazyLoadDBfetch() call
  if (r_typeof(obj) == PROMSXP) {
    obj = r_eval(obj, r_envs.empty);
  }
  if (obj != r_syms.unbound) {
    FREE(1);
    return obj;
  }

  // Trigger object not found error
  r_eval(r_sym(name), env);
  r_stop_unreachable();
}
r_obj* r_base_ns_get(const char* name) {
  return ns_env_get(r_envs.base, name);
}


r_obj* rlang_ns_env = NULL;

r_obj* rlang_ns_get(const char* name) {
  return ns_env_get(rlang_ns_env, name);
}


static r_obj* new_env_call = NULL;
static r_obj* new_env__parent_node = NULL;
static r_obj* new_env__size_node = NULL;

r_obj* r_alloc_environment(r_ssize size, r_obj* parent) {
  parent = parent ? parent : r_envs.empty;
  r_node_poke_car(new_env__parent_node, parent);

  size = size ? size : 29;
  r_node_poke_car(new_env__size_node, r_int(size));

  r_obj* env = r_eval(new_env_call, r_envs.base);

  // Free for gc
  r_node_poke_car(new_env__parent_node, r_null);

  return env;
}


static r_obj* env2list_call = NULL;
static r_obj* list2env_call = NULL;

r_obj* r_env_as_list_compat(r_obj* env, r_obj* out);

r_obj* r_env_as_list(r_obj* env) {
  r_obj* out = KEEP(eval_with_x(env2list_call, env));

#if R_VERSION < R_Version(4, 0, 0)
  out = r_env_as_list_compat(env, out);
#endif

  FREE(1);
  return out;
}

// On R < 4.0, the active binding function is returned instead of
// its value. We invoke the active bindings here to get consistent
// behaviour in all supported R versions.
r_obj* r_env_as_list_compat(r_obj* env, r_obj* out) {
  r_obj* nms = KEEP(r_env_names(env));
  r_obj* types = KEEP(r_env_binding_types(env, nms));

  if (types == R_NilValue) {
    FREE(2);
    return out;
  }

  r_ssize n = r_length(nms);
  r_obj* const * p_nms = r_chr_cbegin(nms);
  const int* p_types = r_int_cbegin(types);

  for (r_ssize i = 0; i < n; ++i) {
    enum r_env_binding_type type = p_types[i];
    if (type == R_ENV_BINDING_TYPE_active) {
      r_ssize fn_idx = r_chr_detect_index(nms, r_str_c_string(p_nms[i]));
      if (fn_idx < 0) {
        r_abort("Internal error: Can't find active binding in list");
      }

      r_obj* fn = r_list_get(out, fn_idx);
      r_obj* value = r_eval(KEEP(r_call(fn)), r_envs.empty);
      r_list_poke(out, fn_idx, value);
      FREE(1);
    }
  }

  FREE(2);
  return out;
}

r_obj* r_env_clone(r_obj* env, r_obj* parent) {
  if (parent == NULL) {
    parent = r_env_parent(env);
  }

  r_obj* nms = KEEP(r_env_names(env));
  r_obj* types = KEEP(r_env_binding_types(env, nms));

  if (types == r_null) {
    FREE(2);
    return env_clone_roundtrip(env, parent);
  }

  r_ssize n = r_length(nms);

#if R_VERSION < R_Version(4, 0, 0)
  // In older R versions there is no way of accessing the function of
  // an active binding except through env2list. This makes it
  // impossible to preserve active bindings without forcing promises.

  r_obj* env_list = KEEP(eval_with_x(env2list_call, env));
  r_obj* out = KEEP(r_list_as_environment(env_list, parent));
#else
  r_obj* out = KEEP(r_alloc_environment(n, parent));
  KEEP(r_null);
#endif

  r_obj* const * v_nms = r_chr_cbegin(nms);
  enum r_env_binding_type* v_types = (enum r_env_binding_type*) r_int_begin(types);

  for (r_ssize i = 0; i < n; ++i, ++v_nms, ++v_types) {
    r_obj* sym = r_str_as_symbol(*v_nms);

    switch (*v_types) {
    case R_ENV_BINDING_TYPE_value:
    case R_ENV_BINDING_TYPE_promise:
      r_env_poke(out, sym, r_env_find(env, sym));
      break;

    case R_ENV_BINDING_TYPE_active: {
#if R_VERSION < R_Version(4, 0, 0)
      r_ssize fn_idx = r_chr_detect_index(nms, r_sym_c_string(sym));
      if (fn_idx < 0) {
        r_abort("Internal error: Can't find active binding in temporary list");
      }
      r_obj* fn = r_list_get(env_list, fn_idx);
#else
      r_obj* fn = R_ActiveBindingFunction(sym, env);
#endif
      r_env_poke_active(out, sym, fn);
      break;
    }}
  }

  FREE(4);
  return out;
}

static
r_obj* env_clone_roundtrip(r_obj* env, r_obj* parent) {
  r_obj* out_list = KEEP(r_env_as_list(env));
  r_obj* out = r_list_as_environment(out_list, parent);
  FREE(1);
  return(out);
}

r_obj* r_list_as_environment(r_obj* x, r_obj* parent) {
  parent = parent ? parent : r_envs.empty;
  return eval_with_xy(list2env_call, x, parent);
}

static r_obj* poke_lazy_call = NULL;
static r_obj* poke_lazy_value_node = NULL;

void r_env_poke_lazy(r_obj* env, r_obj* sym, r_obj* expr, r_obj* eval_env) {
  KEEP(expr);
  r_obj* name = KEEP(r_sym_as_utf8_character(sym));

  r_node_poke_car(poke_lazy_value_node, expr);
  r_eval_with_xyz(poke_lazy_call, name, env, eval_env, rlang_ns_env);
  r_node_poke_car(poke_lazy_value_node, r_null);

  FREE(2);
}


#if RLANG_USE_R_EXISTS
bool r__env_has(r_obj* env, r_obj* sym) {
  r_obj* nm = KEEP(r_sym_as_utf8_character(sym));
  r_obj* out = eval_with_xyz(exists_call, env, nm, r_false);
  FREE(1);
  return r_as_bool(out);
}

bool r__env_has_anywhere(r_obj* env, r_obj* sym) {
  r_obj* nm = KEEP(r_sym_as_utf8_character(sym));
  r_obj* out = eval_with_xyz(exists_call, env, nm, r_true);
  FREE(1);
  return r_as_bool(out);
}
#endif

#if (R_VERSION < R_Version(4, 0, 0))
void r__env_unbind(r_obj* env, r_obj* sym) {
  // Check if binding exists to avoid `rm()` warning
  if (r_env_has(env, sym)) {
    r_obj* nm = KEEP(r_sym_as_utf8_character(sym));
    eval_with_xyz(remove_call, env, nm, r_false);
    FREE(1);
  }
}
#endif


bool r_env_inherits(r_obj* env, r_obj* ancestor, r_obj* top) {
  top = top ? top : r_envs.empty;

  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment");
  }
  if (r_typeof(ancestor) != R_TYPE_environment) {
    r_abort("`ancestor` must be an environment");
  }
  if (r_typeof(top) != R_TYPE_environment) {
    r_abort("`top` must be an environment");
  }

  if (env == r_envs.empty) {
    return false;
  }

  while (env != top && env != r_envs.empty) {
    if (env == ancestor) {
      return true;
    }
    env = r_env_parent(env);;
  }

  return env == ancestor;
}

r_obj* r_env_find_until(r_obj* env, r_obj* sym, r_obj* last) {
  r_obj* stop = r_envs.empty;
  if (last != r_envs.empty) {
    stop = r_env_parent(last);
  }

  r_obj* out = r_syms.unbound;
  while (out == r_syms.unbound && env != r_envs.empty && env != stop) {
    out = r_env_find(env, sym);
    env = r_env_parent(env);
  }

  return out;
}


void r_init_rlang_ns_env() {
  rlang_ns_env = r_ns_env("rlang");
}

r_obj* r_methods_ns_env = NULL;

void r_init_library_env() {
  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", r_envs.base);
  r_preserve(new_env_call);

  new_env__parent_node = r_node_cddr(new_env_call);
  new_env__size_node = r_node_cdr(new_env__parent_node);

  env2list_call = r_parse("as.list.environment(x, all.names = TRUE)");
  r_preserve(env2list_call);

  list2env_call = r_parse("list2env(x, envir = NULL, parent = y, hash = TRUE)");
  r_preserve(list2env_call);

  poke_lazy_call = r_parse("delayedAssign(x, value = NULL, assign.env = y, eval.env = z)");
  r_preserve(poke_lazy_call);

  poke_lazy_value_node = r_node_cddr(poke_lazy_call);

  exists_call = r_parse("exists(y, envir = x, inherits = z)");
  r_preserve(exists_call);

  remove_call = r_parse("remove(list = y, envir = x, inherits = z)");
  r_preserve(remove_call);

  r_methods_ns_env = r_parse_eval("asNamespace('methods')", r_envs.base);
}


static
r_obj* exists_call = NULL;

static
r_obj* remove_call = NULL;
