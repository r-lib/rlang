#ifndef RLANG_ENV_BINDING_H
#define RLANG_ENV_BINDING_H


enum r_env_binding_type {
  R_ENV_BINDING_VALUE = 0,
  R_ENV_BINDING_PROMISE,
  R_ENV_BINDING_ACTIVE
};

bool r_env_binding_is_promise(r_obj* env, r_obj* sym);
bool r_env_binding_is_active(r_obj* env, r_obj* sym);
r_obj* r_env_binding_types(r_obj* env, r_obj* bindings);


#endif
