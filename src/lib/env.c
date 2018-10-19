#include "rlang.h"

sexp* eval_with_x(sexp* call, sexp* x);
sexp* eval_with_xy(sexp* call, sexp* x, sexp* y);
sexp* eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z);


sexp* r_ns_env(const char* pkg) {
  sexp* ns = r_env_find(R_NamespaceRegistry, r_sym(pkg));
  if (ns == r_unbound_sym) {
    r_abort("Can't find namespace `%s`", pkg);
  }
  return ns;
}

static sexp* ns_env_get(sexp* env, const char* name) {
  sexp* obj = KEEP(r_env_find(env, r_sym(name)));

  // Can be a promise to a lazyLoadDBfetch() call
  if (r_typeof(obj) == PROMSXP) {
    obj = r_eval(obj, r_empty_env);
  }
  if (obj != r_unbound_sym) {
    FREE(1);
    return obj;
  }

  // Trigger object not found error
  r_eval(r_sym(name), env);
  r_abort("Internal error: `ns_env_get()` should have failed earlier");
}
sexp* r_base_ns_get(const char* name) {
  return ns_env_get(r_base_env, name);
}


static sexp* rlang_ns_env = NULL;

sexp* rlang_ns_get(const char* name) {
  return ns_env_get(rlang_ns_env, name);
}


static sexp* new_env_call = NULL;
static sexp* new_env__parent_node = NULL;
static sexp* new_env__size_node = NULL;

sexp* r_new_environment(sexp* parent, r_ssize size) {
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

sexp* r_env_as_list(sexp* x) {
  return eval_with_x(env2list_call, x);
}
sexp* r_list_as_environment(sexp* x, sexp* parent) {
  parent = parent ? parent : r_empty_env;
  return eval_with_xy(list2env_call, x, parent);
}

sexp* r_env_clone(sexp* env, sexp* parent) {
  if (parent == NULL) {
    parent = r_env_parent(env);
  }

  sexp* nms = KEEP(r_env_names(env));
  sexp* types = KEEP(r_env_binding_types(env, nms));

  sexp* out_list = KEEP(r_env_as_list(env));
  sexp* out = KEEP(r_list_as_environment(out_list, parent));

  if (types == r_null) {
    FREE(4);
    return out;
  }

  r_ssize n = r_length(nms);
  sexp** nms_ptr = r_chr_deref(nms);
  int* types_ptr = r_int_deref(types);

  // There is currently no way of accessing the function of an active
  // binding except through env2list. This makes it impossible to
  // preserve active bindings without forcing promises.

  for (r_ssize i = 0; i < n; ++i, ++nms_ptr, ++types_ptr) {
    enum r_env_binding_type type = *types_ptr;
    if (type == R_ENV_BINDING_ACTIVE) {
      sexp* str = *nms_ptr;
      sexp* sym = r_str_as_symbol(str);

      r_ssize fn_idx = r_chr_detect_index(nms, r_str_deref(str));
      if (fn_idx < 0) {
        r_abort("Internal error: Can't find active binding in temporary list");
      }

      sexp* nms = KEEP(r_str_as_character(str));
      r_env_unbind_names(out, nms, false);
      FREE(1);

      sexp* fn = r_list_get(out_list, fn_idx);
      R_MakeActiveBinding(sym, fn, out);
    }
  }

  FREE(4);
  return out;
}


static sexp* remove_call = NULL;

sexp* r_env_unbind_names(sexp* env, sexp* names, bool inherits) {
  return eval_with_xyz(remove_call, env, names, inherits ? r_shared_true : r_shared_false);
}

sexp* rlang_env_unbind(sexp* env, sexp* names, sexp* inherits) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment");
  }
  if (r_typeof(names) != r_type_character) {
    r_abort("`names` must be a character vector");
  }
  if (!r_is_scalar_logical(inherits)) {
    r_abort("`inherits` must be a scalar logical vector");
  }
  return r_env_unbind_names(env, names, *r_lgl_deref(inherits));
}

sexp* r_env_unbind_all(sexp* env, const char** names, bool inherits) {
  return r_env_unbind_names(env, r_new_character(names), inherits);
}

sexp* r_env_unbind(sexp* env, const char* name, bool inherits) {
  static const char* names[2] = { "", NULL };
  names[0] = name;
  return r_env_unbind_all(env, names, inherits);
}


void r_init_rlang_ns_env() {
  rlang_ns_env = r_ns_env("rlang");
}

sexp* r_methods_ns_env = NULL;

void r_init_library_env() {
  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", r_base_env);
  r_mark_precious(new_env_call);

  new_env__parent_node = r_node_cddr(new_env_call);
  new_env__size_node = r_node_cdr(new_env__parent_node);

  env2list_call = r_parse("as.list.environment(x, all.names = TRUE)");
  r_mark_precious(env2list_call);

  list2env_call = r_parse("list2env(x, envir = NULL, parent = y, hash = TRUE)");
  r_mark_precious(list2env_call);

  remove_call = r_parse("remove(list = y, envir = x, inherits = z)");
  r_mark_precious(remove_call);

  r_methods_ns_env = r_parse_eval("asNamespace('methods')", r_base_env);
}
