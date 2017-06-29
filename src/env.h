#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>


#define r_base_env R_BaseEnv
#define r_empty_env R_EmptyEnv

bool r_is_unbound_value(SEXP x);
void r_mut_env_parent(SEXP env, SEXP new_parent);
bool r_is_env(SEXP x);
SEXP r_env_get(SEXP env, SEXP sym);
SEXP r_env_set(SEXP env, SEXP sym, SEXP value);
SEXP r_ns_env(const char* pkg);


#endif
