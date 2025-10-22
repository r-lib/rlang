#include <rlang.h>

#include "decl/env-decl.h"

void r_env_unbind_anywhere(r_obj* env, r_obj* sym) {
  while (env != r_envs.empty) {
    if (r_env_has(env, sym)) {
      r_env_unbind(env, sym);
      return;
    }

    env = r_env_parent(env);
  }
}

static
void env_unbind_names(r_obj* env, r_obj* names, bool inherit) {
  r_obj* const * p_names = r_chr_cbegin(names);
  r_ssize n = r_length(names);

  if (inherit) {
    for (r_ssize i = 0; i < n; ++i) {
      r_obj* sym = r_str_as_symbol(p_names[i]);
      r_env_unbind_anywhere(env, sym);
    }
  } else {
    for (r_ssize i = 0; i < n; ++i) {
      r_obj* sym = r_str_as_symbol(p_names[i]);
      r_env_unbind(env, sym);
    }
  }
}

void r_env_unbind_names(r_obj* env, r_obj* names) {
  env_unbind_names(env, names, false);
}
void r_env_unbind_anywhere_names(r_obj* env, r_obj* names) {
  env_unbind_names(env, names, true);
}

void r_env_unbind_c_strings(r_obj* env, const char** names, r_ssize n) {
  r_obj* nms = KEEP(r_chr_n(names, n));
  r_env_unbind_names(env, nms);
  FREE(1);
}
void r_env_unbind_anywhere_c_strings(r_obj* env, const char** names, r_ssize n) {
  r_obj* nms = KEEP(r_chr_n(names, n));
  r_env_unbind_anywhere_names(env, nms);
  FREE(1);
}

void r_env_unbind_c_string(r_obj* env, const char* name) {
  static const char* names[1] = { "" };
  names[0] = name;
  r_env_unbind_c_strings(env, names, 1);
}
void r_env_unbind_anywhere_c_string(r_obj* env, const char* name) {
  static const char* names[1] = { "" };
  names[0] = name;
  r_env_unbind_anywhere_c_strings(env, names, 1);
}

void r_env_poke_parent(r_obj* env, r_obj* new_parent) {
  r_eval_with_xy(env_poke_parent_call, env, new_parent, r_envs.base);
}

r_obj* ffi_env_coalesce(r_obj* env, r_obj* from) {
  r_env_coalesce(env, from);
  return r_null;
}

void rlang_init_env(void) {
  env_poke_parent_call = r_parse("`parent.env<-`(x, y)");
  r_preserve(env_poke_parent_call);
}

static r_obj* env_poke_parent_call = NULL;
