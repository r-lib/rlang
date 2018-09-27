#ifndef RLANG_EVAL_H
#define RLANG_EVAL_H


static inline sexp* r_eval(sexp* expr, sexp* env) {
  return Rf_eval(expr, env);
}

sexp* r_eval_with_x(sexp* call, sexp* parent, sexp* x);


#endif
