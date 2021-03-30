#ifndef RLANG_FN_H
#define RLANG_FN_H


static inline
sexp* r_fn_body(sexp* fn) {
  return BODY_EXPR(fn);
}
static inline
void r_fn_poke_body(sexp* fn, sexp* body) {
  SET_BODY(fn, body);
}

static inline
sexp* r_fn_env(sexp* fn) {
  return CLOENV(fn);
}
static inline
void r_fn_poke_env(sexp* fn, sexp* env) {
  SET_CLOENV(fn, env);
}

static inline
sexp* r_new_function(sexp* formals, sexp* body, sexp* env) {
  SEXP fn = Rf_allocSExp(R_TYPE_closure);
  SET_FORMALS(fn, formals);
  SET_BODY(fn, body);
  SET_CLOENV(fn, env);
  return fn;
}

sexp* r_as_function(sexp* x, const char* arg);

static inline
bool r_is_function(sexp* x) {
  switch (r_typeof(x)) {
  case R_TYPE_closure:
  case R_TYPE_builtin:
  case R_TYPE_special:
    return true;
  default:
    return false;
  }
}

static inline
bool r_is_primitive(sexp* x) {
  switch (r_typeof(x)) {
  case R_TYPE_builtin:
  case R_TYPE_special:
    return true;
  default:
    return false;
  }
}


#endif
