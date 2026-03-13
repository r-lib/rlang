#ifndef RLANG_DOTS_API_H
#define RLANG_DOTS_API_H

#include <rlang.h>

// Dots API - mirrors R-devel PR #209 ---

typedef enum {
    DOT_TYPE_value = 0,
    DOT_TYPE_missing = 1,
    DOT_TYPE_delayed = 2,
    DOT_TYPE_forced = 3
} r_dot_type_t;

Rboolean r_env_dots_exist(SEXP env);
int r_env_dots_length(SEXP env);
SEXP r_env_dots_names(SEXP env);
SEXP r_env_dot_get(SEXP env, r_ssize i);

r_dot_type_t r_env_dot_type(SEXP env, r_ssize i);

SEXP r_env_dot_delayed_expr(SEXP env, r_ssize i);
SEXP r_env_dot_delayed_env(SEXP env, r_ssize i);
SEXP r_env_dot_forced_expr(SEXP env, r_ssize i);


// FFI wrappers for R interface ---

SEXP ffi_dots_exist(SEXP env);
SEXP ffi_dots_length(SEXP env);
SEXP ffi_dots_names(SEXP env);
SEXP ffi_dot_get(SEXP ffi_i, SEXP env);
SEXP ffi_dot_type(SEXP ffi_i, SEXP env);
SEXP ffi_dot_delayed_expr(SEXP ffi_i, SEXP env);
SEXP ffi_dot_delayed_env(SEXP ffi_i, SEXP env);
SEXP ffi_dot_forced_expr(SEXP ffi_i, SEXP env);

#endif /* RLANG_DOTS_API_H */
