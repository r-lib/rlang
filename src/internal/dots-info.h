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

bool r_env_dots_exist(r_obj* env);
r_ssize r_env_dots_length(r_obj* env);
r_obj* r_env_dots_names(r_obj* env);
r_obj* r_env_dot_get(r_obj* env, r_ssize i);

r_dot_type_t r_env_dot_type(r_obj* env, r_ssize i);

r_obj* r_env_dot_delayed_expr(r_obj* env, r_ssize i);
r_obj* r_env_dot_delayed_env(r_obj* env, r_ssize i);
r_obj* r_env_dot_forced_expr(r_obj* env, r_ssize i);


// FFI wrappers for R interface ---

r_obj* ffi_dots_exist(r_obj* env);
r_obj* ffi_dots_length(r_obj* env);
r_obj* ffi_dots_names(r_obj* env);
r_obj* ffi_dot_get(r_obj* ffi_i, r_obj* env);
r_obj* ffi_dot_type(r_obj* ffi_i, r_obj* env);
r_obj* ffi_dot_delayed_expr(r_obj* ffi_i, r_obj* env);
r_obj* ffi_dot_delayed_env(r_obj* ffi_i, r_obj* env);
r_obj* ffi_dot_forced_expr(r_obj* ffi_i, r_obj* env);

#endif /* RLANG_DOTS_API_H */
