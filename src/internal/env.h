#ifndef RLANG_INTERNAL_ENV_H
#define RLANG_INTERNAL_ENV_H

#include <rlang.h>

void r_env_unbind_anywhere(r_obj* env, r_obj* sym);

void r_env_unbind_anywhere_names(r_obj* env, r_obj* names);
void r_env_unbind_anywhere_c_string(r_obj* env, const char* name);
void r_env_unbind_anywhere_c_strings(r_obj* env, const char** names, r_ssize n);

void r_env_unbind_names(r_obj* env, r_obj* names);
void r_env_unbind_c_string(r_obj* env, const char* name);
void r_env_unbind_c_strings(r_obj* env, const char** strings, r_ssize n);

// Not part of the rlang C API.
// Maybe one day we will get a blessed C API for `SET_ENCLOS()`.
void r_env_poke_parent(r_obj* env, r_obj* new_parent);

#endif
