#ifndef RLANG_FN_H
#define RLANG_FN_H


static inline SEXP r_fn_body(SEXP x) {
  return BODY_EXPR(x);
}


#endif
