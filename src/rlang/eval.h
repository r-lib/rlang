#ifndef RLANG_EVAL_H
#define RLANG_EVAL_H


static inline sexp* r_eval(sexp* expr, sexp* env) {
  return Rf_eval(expr, env);
}

sexp* r_eval_with_x(sexp* call, sexp* parent, sexp* x);
sexp* r_eval_with_xy(sexp* call, sexp* parent, sexp* x, sexp* y);
sexp* r_eval_with_xyz(sexp* call, sexp* parent, sexp* x, sexp* y, sexp* z);
sexp* r_eval_with_wxyz(sexp* call, sexp* parent, sexp* w, sexp* x, sexp* y, sexp* z);

sexp* r_eval_in_with_x(sexp* call, sexp* env,
                       sexp* x, sexp* x_sym);
sexp* r_eval_in_with_xy(sexp* call, sexp* env,
                        sexp* x, sexp* x_sym,
                        sexp* y, sexp* y_sym);
sexp* r_eval_in_with_xyz(sexp* call, sexp* env,
                         sexp* x, sexp* x_sym,
                         sexp* y, sexp* y_sym,
                         sexp* z, sexp* z_sym);
sexp* r_eval_in_with_wxyz(sexp* call, sexp* env,
                          sexp* w, sexp* w_sym,
                          sexp* x, sexp* x_sym,
                          sexp* y, sexp* y_sym,
                          sexp* z, sexp* z_sym);

#endif
