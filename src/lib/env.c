#include "rlang.h"


sexp* r_ns_env(const char* pkg) {
  sexp* ns = r_env_get(R_NamespaceRegistry, r_sym(pkg));
  if (r_is_unbound_value(ns)) {
    r_abort("Can't find namespace `%s`", pkg);
  }
  return ns;
}

static sexp* ns_env_get(sexp* env, const char* name) {
  sexp* obj = r_env_get(env, r_sym(name));

  // Can be a promise to a lazyLoadDBfetch() call
  if (r_typeof(obj) == PROMSXP) {
    obj = r_eval(obj, r_empty_env);
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

sexp* r_new_environment(sexp* parent) {
  if (!parent) {
    parent = r_empty_env;
  }

  r_node_poke_cadr(new_env_call, parent);
  return r_eval(new_env_call, r_empty_env);
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


void r_init_library_env() {
  new_env_call = rlang_ns_get("rlang_new_env_call");

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
}
