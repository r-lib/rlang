#ifndef RLANG_FN_H
#define RLANG_FN_H


static inline sexp* r_fn_body(sexp* fn) {
  return BODY_EXPR(fn);
}
static inline void r_fn_poke_body(sexp* fn, sexp* body) {
  SET_BODY(fn, body);
}

static inline sexp* r_fn_env(sexp* fn) {
  return CLOENV(fn);
}
static inline void r_fn_poke_env(sexp* fn, sexp* env) {
  SET_CLOENV(fn, env);
}

sexp* r_new_function(sexp* formals, sexp* body, sexp* env);

static inline bool r_is_function(sexp* x) {
  switch (r_typeof(x)) {
  case r_type_closure:
  case r_type_builtin:
  case r_type_special:
    return true;
  default:
    return false;
  }
}


#endif
