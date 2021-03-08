#include "rlang.h"
#include <Rversion.h>

sexp* eval_with_x(sexp* call, sexp* x);
sexp* eval_with_xy(sexp* call, sexp* x, sexp* y);
sexp* eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z);


sexp* r_ns_env(const char* pkg) {
  sexp* ns = r_env_find(R_NamespaceRegistry, r_sym(pkg));
  if (ns == r_syms.unbound) {
    r_abort("Can't find namespace `%s`", pkg);
  }
  return ns;
}

static
sexp* ns_env_get(sexp* env, const char* name) {
  sexp* obj = KEEP(r_env_find(env, r_sym(name)));

  // Can be a promise to a lazyLoadDBfetch() call
  if (r_typeof(obj) == PROMSXP) {
    obj = r_eval(obj, r_empty_env);
  }
  if (obj != r_syms.unbound) {
    FREE(1);
    return obj;
  }

  // Trigger object not found error
  r_eval(r_sym(name), env);
  r_stop_unreached("ns_env_get");
}
sexp* r_base_ns_get(const char* name) {
  return ns_env_get(r_base_env, name);
}


sexp* rlang_ns_env = NULL;

sexp* rlang_ns_get(const char* name) {
  return ns_env_get(rlang_ns_env, name);
}


static sexp* new_env_call = NULL;
static sexp* new_env__parent_node = NULL;
static sexp* new_env__size_node = NULL;

sexp* r_alloc_environment(r_ssize size, sexp* parent) {
  parent = parent ? parent : r_empty_env;
  r_node_poke_car(new_env__parent_node, parent);

  size = size ? size : 29;
  r_node_poke_car(new_env__size_node, r_int(size));

  sexp* env = r_eval(new_env_call, r_base_env);

  // Free for gc
  r_node_poke_car(new_env__parent_node, r_null);

  return env;
}


static sexp* env2list_call = NULL;
static sexp* list2env_call = NULL;

sexp* r_env_as_list_compat(sexp* env, sexp* out);

sexp* r_env_as_list(sexp* env) {
  sexp* out = KEEP(eval_with_x(env2list_call, env));

#if R_VERSION < R_Version(4, 0, 0)
  out = r_env_as_list_compat(env, out);
#endif

  FREE(1);
  return out;
}

// On R < 4.0, the active binding function is returned instead of
// its value. We invoke the active bindings here to get consistent
// behaviour in all supported R versions.
sexp* r_env_as_list_compat(sexp* env, sexp* out) {
  sexp* nms = KEEP(r_env_names(env));
  sexp* types = KEEP(r_env_binding_types(env, nms));

  if (types == R_NilValue) {
    FREE(2);
    return out;
  }

  r_ssize n = r_length(nms);
  sexp* const * p_nms = r_chr_deref_const(nms);
  const int* p_types = r_int_deref_const(types);

  for (r_ssize i = 0; i < n; ++i) {
    enum r_env_binding_type type = p_types[i];
    if (type == R_ENV_BINDING_ACTIVE) {
      r_ssize fn_idx = r_chr_detect_index(nms, r_str_c_string(p_nms[i]));
      if (fn_idx < 0) {
        r_abort("Internal error: Can't find active binding in list");
      }

      sexp* fn = r_list_get(out, fn_idx);
      sexp* value = r_eval(KEEP(r_call(fn)), r_empty_env);
      r_list_poke(out, fn_idx, value);
      FREE(1);
    }
  }

  FREE(2);
  return out;
}

sexp* r_list_as_environment(sexp* x, sexp* parent) {
  parent = parent ? parent : r_empty_env;
  return eval_with_xy(list2env_call, x, parent);
}

sexp* r_env_clone(sexp* env, sexp* parent) {
  if (parent == NULL) {
    parent = r_env_parent(env);
  }

  sexp* out = KEEP(r_env_as_list(env));
  out = r_list_as_environment(out, parent);

  FREE(1);
  return out;
}


static sexp* poke_lazy_call = NULL;
static sexp* poke_lazy_value_node = NULL;

void r_env_poke_lazy(sexp* env, sexp* sym, sexp* expr, sexp* eval_env) {
  sexp* name = KEEP(r_sym_as_character(sym));

  r_node_poke_car(poke_lazy_value_node, expr);
  r_eval_with_xyz(poke_lazy_call, name, env, eval_env, rlang_ns_env);
  r_node_poke_car(poke_lazy_value_node, r_null);

  FREE(1);
}


static sexp* remove_call = NULL;

#if (R_VERSION < R_Version(4, 0, 0))
void r__env_unbind(sexp* env, sexp* sym) {
  // Check if binding exists to avoid `rm()` warning
  if (r_env_has(env, sym)) {
    sexp* nm = KEEP(r_sym_as_character(sym));
    eval_with_xyz(remove_call, env, nm, r_false);
    FREE(1);
  }
}
#endif


bool r_env_inherits(sexp* env, sexp* ancestor, sexp* top) {
  top = top ? top : r_empty_env;

  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment");
  }
  if (r_typeof(ancestor) != R_TYPE_environment) {
    r_abort("`ancestor` must be an environment");
  }
  if (r_typeof(top) != R_TYPE_environment) {
    r_abort("`top` must be an environment");
  }

  if (env == r_empty_env) {
    return false;
  }

  while (env != top && env != r_empty_env) {
    if (env == ancestor) {
      return true;
    }
    env = r_env_parent(env);;
  }

  return env == ancestor;
}

void r_init_rlang_ns_env() {
  rlang_ns_env = r_ns_env("rlang");
}

sexp* r_methods_ns_env = NULL;

void r_init_library_env() {
  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", r_base_env);
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

  remove_call = r_parse("remove(list = y, envir = x, inherits = z)");
  r_preserve(remove_call);

  r_methods_ns_env = r_parse_eval("asNamespace('methods')", r_base_env);
}
