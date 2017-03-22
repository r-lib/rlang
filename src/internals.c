#define R_NO_REMAP
#define USE_RINTERNALS
#include <Rinternals.h>

SEXP rlang_set_parent(SEXP env, SEXP new_parent) {
  ENCLOS(env) = new_parent;
  return env;
}

SEXP rlang_mut_language_type(SEXP node) {
  TYPEOF(node) = LANGSXP;
  return node;
}
SEXP rlang_mut_pairlist_type(SEXP node) {
  TYPEOF(node) = LISTSXP;
  return node;
}
