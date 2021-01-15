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

sexp* r_exec_mask_n(sexp* fn_sym,
                    sexp* fn,
                    const struct r_pair* args,
                    int n,
                    sexp* parent);

sexp* r_exec_mask_n_call_poke(sexp* fn_sym,
                              sexp* fn,
                              const struct r_pair* args,
                              int n,
                              sexp* env);

static inline
sexp* r_exec_mask1(sexp* fn_sym, sexp* fn,
                   sexp* x_sym, sexp* x,
                   sexp* env) {
  struct r_pair args[] = {
    { x_sym, x }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
sexp* r_exec_mask2(sexp* fn_sym, sexp* fn,
                   sexp* x1_sym, sexp* x1,
                   sexp* x2_sym, sexp* x2,
                   sexp* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
sexp* r_exec_mask3(sexp* fn_sym, sexp* fn,
                   sexp* x1_sym, sexp* x1,
                   sexp* x2_sym, sexp* x2,
                   sexp* x3_sym, sexp* x3,
                   sexp* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
sexp* r_exec_mask4(sexp* fn_sym, sexp* fn,
                   sexp* x1_sym, sexp* x1,
                   sexp* x2_sym, sexp* x2,
                   sexp* x3_sym, sexp* x3,
                   sexp* x4_sym, sexp* x4,
                   sexp* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 },
    { x4_sym, x4 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
sexp* r_exec_mask5(sexp* fn_sym, sexp* fn,
                   sexp* x1_sym, sexp* x1,
                   sexp* x2_sym, sexp* x2,
                   sexp* x3_sym, sexp* x3,
                   sexp* x4_sym, sexp* x4,
                   sexp* x5_sym, sexp* x5,
                   sexp* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 },
    { x4_sym, x4 },
    { x5_sym, x5 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
sexp* r_exec_mask6(sexp* fn_sym, sexp* fn,
                   sexp* x1_sym, sexp* x1,
                   sexp* x2_sym, sexp* x2,
                   sexp* x3_sym, sexp* x3,
                   sexp* x4_sym, sexp* x4,
                   sexp* x5_sym, sexp* x5,
                   sexp* x6_sym, sexp* x6,
                   sexp* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 },
    { x4_sym, x4 },
    { x5_sym, x5 },
    { x6_sym, x6 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
sexp* r_exec_mask7(sexp* fn_sym, sexp* fn,
                   sexp* x1_sym, sexp* x1,
                   sexp* x2_sym, sexp* x2,
                   sexp* x3_sym, sexp* x3,
                   sexp* x4_sym, sexp* x4,
                   sexp* x5_sym, sexp* x5,
                   sexp* x6_sym, sexp* x6,
                   sexp* x7_sym, sexp* x7,
                   sexp* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 },
    { x4_sym, x4 },
    { x5_sym, x5 },
    { x6_sym, x6 },
    { x7_sym, x7 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}


#endif
