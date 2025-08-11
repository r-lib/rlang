// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ENV_BINDING_H
#define RLANG_ENV_BINDING_H

#include "rlang-types.h"

enum r_env_binding_type {
  R_ENV_BINDING_TYPE_value = 0,
  R_ENV_BINDING_TYPE_promise,
  R_ENV_BINDING_TYPE_active
};

r_obj* r_env_binding_types(r_obj* env, r_obj* bindings);


#endif
