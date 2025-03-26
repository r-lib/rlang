// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_FN_H
#define RLANG_FN_H

#include "rlang-types.h"
#include "obj.h"

static inline
r_obj* r_fn_body(r_obj* fn) {
  return BODY_EXPR(fn);
}
static inline
void r_fn_poke_body(r_obj* fn, r_obj* body) {
  RLANG_SET_BODY(fn, body);
}

static inline
r_obj* r_fn_env(r_obj* fn) {
  return CLOENV(fn);
}
static inline
void r_fn_poke_env(r_obj* fn, r_obj* env) {
  RLANG_SET_CLOENV(fn, env);
}

static inline
r_obj* r_new_function(r_obj* formals, r_obj* body, r_obj* env) {
  SEXP fn = Rf_allocSExp(R_TYPE_closure);
  RLANG_SET_FORMALS(fn, formals);
  RLANG_SET_BODY(fn, body);
  RLANG_SET_CLOENV(fn, env);
  return fn;
}

r_obj* r_as_function(r_obj* x, const char* arg);

static inline
bool r_is_function(r_obj* x) {
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
bool r_is_primitive(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_builtin:
  case R_TYPE_special:
    return true;
  default:
    return false;
  }
}


#endif
