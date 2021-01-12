#include <rlang.h>

#define FRAME_LOCK_MASK (1 << 14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~FRAME_LOCK_MASK))

// Should only be used in development tools
sexp* rlang_env_unlock(sexp* env) {
  UNLOCK_FRAME(env);
  return FRAME_IS_LOCKED(env) == 0 ? r_true : r_false;
}


void r_env_unbind_anywhere(sexp* env, sexp* sym) {
  while (env != r_empty_env) {
    if (r_env_has(env, sym)) {
      r_env_unbind(env, sym);
      return;
    }

    env = r_env_parent(env);
  }
}

void r_env_unbind_syms(sexp* env, sexp** syms) {
  while (syms) {
    r_env_unbind(env, *syms++);
  }
}

static
void env_unbind_names(sexp* env, sexp* names, bool inherit) {
  sexp* const * p_names = r_chr_deref_const(names);
  r_ssize n = r_length(names);

  if (inherit) {
    for (r_ssize i = 0; i < n; ++i) {
      sexp* sym = r_str_as_symbol(p_names[i]);
      r_env_unbind_anywhere(env, sym);
    }
  } else {
    for (r_ssize i = 0; i < n; ++i) {
      sexp* sym = r_str_as_symbol(p_names[i]);
      r_env_unbind(env, sym);
    }
  }
}

void r_env_unbind_names(sexp* env, sexp* names) {
  env_unbind_names(env, names, false);
}
void r_env_unbind_anywhere_names(sexp* env, sexp* names) {
  env_unbind_names(env, names, true);
}

void r_env_unbind_strings(sexp* env, const char** names) {
  sexp* nms = KEEP(r_chr_n(names));
  r_env_unbind_names(env, nms);
  FREE(1);
}
void r_env_unbind_anywhere_strings(sexp* env, const char** names) {
  sexp* nms = KEEP(r_chr_n(names));
  r_env_unbind_anywhere_names(env, nms);
  FREE(1);
}

void r_env_unbind_string(sexp* env, const char* name) {
  static const char* names[2] = { "", NULL };
  names[0] = name;
  r_env_unbind_strings(env, names);
}
void r_env_unbind_string_anywhere(sexp* env, const char* name) {
  static const char* names[2] = { "", NULL };
  names[0] = name;
  r_env_unbind_anywhere_strings(env, names);
}
