#include "rlang.h"


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
sexp* rlang_ns_get(const char* name) {
  return ns_env_get(r_ns_env("rlang"), name);
}
sexp* r_base_ns_get(const char* name) {
  return ns_env_get(r_base_env, name);
}


static sexp* new_env_call = NULL;

sexp* r_new_environment(sexp* parent, r_ssize_t size) {
  if (!parent) {
    parent = r_empty_env;
  }
  sexp* parent_node = r_node_cdr(new_env_call);
  r_node_poke_car(parent_node, parent);

  if (!size) {
    size = 29;
  }
  sexp* size_node = r_node_cdr(parent_node);
  r_node_poke_car(size_node, r_scalar_int(size));

  sexp* env = r_eval(new_env_call, r_empty_env);
  r_node_poke_car(parent_node, r_null);

  return env;
}


static sexp* env2list_call = NULL;

sexp* r_env_as_list(sexp* x) {
  sexp* arg_node = r_node_cdr(env2list_call);
  r_node_poke_car(arg_node, x);

  sexp* env = r_eval(env2list_call, r_empty_env);

  // Release input node for GC
  r_node_poke_car(arg_node, r_null);

  return env;
}


static sexp* list2env_call = NULL;

sexp* r_list_as_environment(sexp* x, sexp* parent) {
  if (parent == NULL) {
    parent = r_empty_env;
  }

  sexp* input_node = r_node_cdr(list2env_call);
  r_node_poke_car(input_node, x);

  sexp* parent_node = r_node_cddr(input_node);
  r_node_poke_car(parent_node, parent);

  sexp* env = r_eval(list2env_call, r_empty_env);

  // Release input list for GC
  r_node_poke_car(input_node, r_null);
  r_node_poke_car(parent_node, r_null);

  return env;
}

sexp* r_env_clone(sexp* env, sexp* parent) {
  if (parent == NULL) {
    parent = r_env_parent(env);
  }
  sexp* list = KEEP(r_env_as_list(env));
  sexp* clone = r_list_as_environment(list, parent);

  FREE(1);
  return clone;
}


static sexp* remove_call = NULL;

sexp* r_env_unbind_names(sexp* env, sexp* names, bool inherits) {
  sexp* names_node = r_node_cdr(remove_call);
  r_node_poke_car(names_node, names);

  sexp* env_node = r_node_cdr(names_node);
  r_node_poke_car(env_node, env);

  sexp* inherits_node = r_node_cdr(env_node);
  r_node_poke_car(inherits_node, r_scalar_lgl(inherits));


  // Evaluate call and free arguments for GC
  r_eval(remove_call, r_base_env);
  r_node_poke_car(names_node, r_null);
  r_node_poke_car(env_node, r_null);

  return env;
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

sexp* r_env_unbind_all(sexp* env, const char** names, r_ssize_t n, bool inherits) {
  return r_env_unbind_names(env, r_new_character(names, n), inherits);
}

sexp* r_env_unbind(sexp* env, const char* name, bool inherits) {
  return r_env_unbind_all(env, &name, 1, inherits);
}


void r_init_library_env() {
  sexp* new_env_args = r_null;
  sexp* hash = KEEP(r_scalar_lgl(1));
  new_env_args = KEEP(r_new_tagged_node("hash", hash, new_env_args));
  new_env_args = KEEP(r_new_tagged_node("size", r_null, new_env_args));
  new_env_args = KEEP(r_new_tagged_node("parent", r_null, new_env_args));
  new_env_call = r_new_call_node(r_base_ns_get("new.env"), new_env_args);
  r_mark_precious(new_env_call);
  FREE(4);

  sexp* env2list_args;
  env2list_args = KEEP(r_scalar_lgl(1));
  env2list_args = KEEP(r_new_tagged_node("all.names", env2list_args, r_null));
  env2list_args = KEEP(r_new_tagged_node("x", r_null, env2list_args));
  env2list_call = r_new_call_node(r_base_ns_get("as.list.environment"), env2list_args);
  r_mark_precious(env2list_call);
  FREE(3);

  sexp* list2env_args;
  list2env_args = KEEP(r_scalar_lgl(1));
  list2env_args = KEEP(r_new_tagged_node("hash", list2env_args, r_null));
  list2env_args = KEEP(r_new_tagged_node("parent", r_null, list2env_args));
  list2env_args = KEEP(r_new_tagged_node("envir", r_null, list2env_args));
  list2env_args = KEEP(r_new_tagged_node("x", r_null, list2env_args));
  list2env_call = r_new_call_node(r_base_ns_get("list2env"), list2env_args);
  r_mark_precious(list2env_call);
  FREE(5);

  sexp* remove_args = r_null;
  sexp* inherits = KEEP(r_scalar_lgl(0));
  remove_args = KEEP(r_new_tagged_node("inherits", inherits, remove_args));
  remove_args = KEEP(r_new_tagged_node("envir", r_null, remove_args));
  remove_args = KEEP(r_new_tagged_node("list", r_null, remove_args));
  remove_call = r_new_call_node(r_base_ns_get("remove"), remove_args);
  r_mark_precious(remove_call);
  FREE(4);
}
