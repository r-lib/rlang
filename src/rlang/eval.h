#ifndef RLANG_EVAL_H
#define RLANG_EVAL_H


static inline SEXP r_eval(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}


#endif
