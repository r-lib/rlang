#define R_NO_REMAP
#include <Rinternals.h>

SEXP r_mut_env_parent(SEXP env, SEXP new_parent) {
  SET_ENCLOS(env, new_parent);
  return env;
}
