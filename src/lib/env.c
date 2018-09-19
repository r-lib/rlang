#include "rlang.h"

static sexp* eval_with_x(sexp* call, sexp* x);
static sexp* eval_with_xy(sexp* call, sexp* x, sexp* y);
static sexp* eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z);


sexp* r_ns_env(const char* pkg) {
  sexp* ns = r_env_get(R_NamespaceRegistry, r_sym(pkg));
  if (ns == r_unbound_sym) {
    r_abort("Can't find namespace `%s`", pkg);
  }
  return ns;
}

static sexp* ns_env_get(sexp* env, const char* name) {
  sexp* obj = r_env_get(env, r_sym(name));

  // Can be a promise to a lazyLoadDBfetch() call
  if (r_typeof(obj) == PROMSXP) {
    KEEP(obj); // Help rchk
    obj = r_eval(obj, r_empty_env);
    FREE(1);
  }

  return obj;
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

sexp* r_new_environment(sexp* parent, r_ssize_t size) {
  parent = parent ? parent : r_empty_env;
  r_node_poke_car(new_env__parent_node, parent);

  size = size ? size : 29;
  r_node_poke_car(new_env__size_node, r_scalar_int(size));

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
  sexp* out = KEEP(r_new_environment(parent, 0));
  sexp* frame = KEEP(r_duplicate(FRAME(env), true));
  sexp* hashtab = KEEP(r_duplicate(HASHTAB(env), true));

  SET_FRAME(out, frame);
  SET_HASHTAB(out, hashtab);

  FREE(3);
  return out;
}


static sexp* remove_call = NULL;
static sexp* remove__list_node = NULL;
static sexp* remove__envir_node = NULL;
static sexp* remove__inherits_node = NULL;

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
}
