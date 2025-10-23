// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_FN_H
#define RLANG_FN_H

#include "rlang-types.h"
#include "obj.h"

static inline
r_obj* r_fn_formals(r_obj* fn) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureFormals(fn);
#else
  return FORMALS(fn);
#endif
}

// Identical to `R_BytecodeExpr(R_ClosureBody(fn))`, which we always want
// since it matches the R level `body()`
static inline
r_obj* r_fn_body(r_obj* fn) {
  return R_ClosureExpr(fn);
}

static inline
r_obj* r_fn_env(r_obj* fn) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ClosureEnv(fn);
#else
  return CLOENV(fn);
#endif
}

static inline
r_obj* r_new_function(r_obj* formals, r_obj* body, r_obj* env) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_mkClosure(formals, body, env);
#else
  SEXP fn = Rf_allocSExp(R_TYPE_closure);
  SET_FORMALS(fn, formals);
  SET_BODY(fn, body);
  SET_CLOENV(fn, env);
  return fn;
#endif
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
