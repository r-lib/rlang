#ifndef RLANG_ENV_BINDING_H
#define RLANG_ENV_BINDING_H


enum r_env_binding_type {
  R_ENV_BINDING_TYPE_value = 0,
  R_ENV_BINDING_TYPE_promise,
  R_ENV_BINDING_TYPE_active
};

bool r_env_binding_is_promise(r_obj* env, r_obj* sym);
bool r_env_binding_is_active(r_obj* env, r_obj* sym);
r_obj* r_env_binding_types(r_obj* env, r_obj* bindings);


#endif
