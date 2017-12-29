#ifndef RLANG_FN_H
#define RLANG_FN_H


static inline sexp* r_fn_body(sexp* x) {
  return BODY_EXPR(x);
}


#endif
