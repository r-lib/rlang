#include "rlang.h"

SEXP r_mut_env_parent(SEXP env, SEXP new_parent) {
  SET_ENCLOS(env, new_parent);
  return env;
}
