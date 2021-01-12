#ifndef RLANG_EVAL_H
#define RLANG_EVAL_H


static inline
sexp* r_eval(sexp* expr, sexp* env) {
  return Rf_eval(expr, env);
}

sexp* r_eval_with_x(sexp* call, sexp* x, sexp* parent);
sexp* r_eval_with_xy(sexp* call, sexp* x, sexp* y, sexp* parent);
sexp* r_eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z, sexp* parent);
sexp* r_eval_with_wxyz(sexp* call, sexp* w, sexp* x, sexp* y, sexp* z, sexp* parent);


#endif
