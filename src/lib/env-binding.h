#ifndef RLANG_ENV_BINDING_H
#define RLANG_ENV_BINDING_H


bool r_env_binding_is_promise(sexp* env, sexp* sym);
bool r_env_binding_is_active(sexp* env, sexp* sym);


#endif
