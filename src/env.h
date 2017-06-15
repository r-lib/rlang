#ifndef RLANG_ENV_H
#define RLANG_ENV_H

#include <stdbool.h>


bool r_is_env(SEXP x);
SEXP r_env_get(SEXP env, SEXP sym);
SEXP r_base_env();


#endif
