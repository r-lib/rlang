#ifndef RLANG_ENV_BINDING_H
#define RLANG_ENV_BINDING_H


enum r_env_binding_type {
  R_ENV_BINDING_VALUE = 0,
  R_ENV_BINDING_PROMISE,
  R_ENV_BINDING_ACTIVE
};

bool r_env_binding_is_promise(sexp* env, sexp* sym);
bool r_env_binding_is_active(sexp* env, sexp* sym);
sexp* r_env_binding_types(sexp* env, sexp* bindings);


#endif
