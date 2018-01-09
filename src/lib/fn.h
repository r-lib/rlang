#ifndef RLANG_FN_H
#define RLANG_FN_H


static inline sexp* r_fn_body(sexp* fn) {
  return BODY_EXPR(fn);
}
static inline sexp* r_fn_poke_body(sexp* fn, sexp* body) {
  SET_BODY(fn, body);
  return fn;
}

sexp* r_new_function(sexp* formals, sexp* body, sexp* env);


#endif
