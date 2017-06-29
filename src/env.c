#include "rlang.h"


bool r_is_unbound_value(SEXP x) {
  return x == R_UnboundValue;
}

void r_mut_env_parent(SEXP env, SEXP new_parent) {
  SET_ENCLOS(env, new_parent);
}
SEXP rlang_mut_env_parent(SEXP env, SEXP new_parent) {
  SET_ENCLOS(env, new_parent);
  return env;
}

bool r_is_env(SEXP x) {
  return r_typeof(x) == ENVSXP;
}

SEXP r_env_get(SEXP env, SEXP sym) {
  return Rf_findVarInFrame3(env, sym, TRUE);
}
SEXP r_env_set(SEXP env, SEXP sym, SEXP value) {
  Rf_defineVar(sym, value, env);
  return env;
}

SEXP r_ns_env(const char* pkg) {
  SEXP ns = r_env_get(R_NamespaceRegistry, r_sym(pkg));
  if (r_is_unbound_value(ns))
    r_abort("Can't find namespace `%s`", pkg);
  return ns;
}
