#include "rlang.h"

r_obj* ffi_has_dots_unnamed(r_obj* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` is a not an environment.");
  }

  if (!r_env_dots_exist(env)) {
    r_abort("No `...` found.");
  }

  int n = r_env_dots_length(env);

  // Empty dots count as unnamed
  if (n == 0) {
    return r_true;
  }

  r_obj* names = KEEP(r_env_dots_names(env));
  r_obj* unnamed = (names == r_null) ? r_true : r_false;
  FREE(1);
  return unnamed;
}

r_obj* ffi_ellipsis_dots_used(r_obj* env) {
  if (!r_env_dots_exist(env)) {
    return r_true;
  }

  int n = r_env_dots_length(env);

  // Empty or missing dots count as used
  if (n == 0) {
    return r_true;
  }

  for (int i = 0; i < n; ++i) {
    r_dot_type_t type = r_env_dot_type(env, i);
    if (type == DOT_TYPE_delayed) {
      return r_false;
    }
  }

  return r_true;
}
