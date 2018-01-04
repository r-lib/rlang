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


void r_init_library_env() {
  new_env_call = rlang_ns_get("rlang_new_env_call");
}
