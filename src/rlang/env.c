#include "rlang.h"


SEXP r_ns_env(const char* pkg) {
  SEXP ns = r_env_get(R_NamespaceRegistry, r_sym(pkg));
  if (r_is_unbound_value(ns))
    r_abort("Can't find namespace `%s`", pkg);
  return ns;
}
