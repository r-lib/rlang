#include "rlang.h"


sexp* r_ns_env(const char* pkg) {
  sexp* ns = r_env_get(R_NamespaceRegistry, r_sym(pkg));
  if (r_is_unbound_value(ns))
    r_abort("Can't find namespace `%s`", pkg);
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
