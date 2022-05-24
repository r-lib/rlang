#ifndef RLANG_EVAL_H
#define RLANG_EVAL_H


static inline
r_obj* r_eval(r_obj* expr, r_obj* env) {
  return Rf_eval(expr, env);
}

r_obj* r_eval_with_x(r_obj* call, r_obj* x, r_obj* parent);
r_obj* r_eval_with_xy(r_obj* call, r_obj* x, r_obj* y, r_obj* parent);
r_obj* r_eval_with_xyz(r_obj* call, r_obj* x, r_obj* y, r_obj* z, r_obj* parent);
r_obj* r_eval_with_wxyz(r_obj* call, r_obj* w, r_obj* x, r_obj* y, r_obj* z, r_obj* parent);

r_obj* r_exec_mask_n(r_obj* fn_sym,
                     r_obj* fn,
                     const struct r_pair* args,
                     int n,
                     r_obj* parent);

r_obj* r_exec_n(r_obj* fn_sym,
                r_obj* fn,
                const struct r_pair* args,
                int n,
                r_obj* env);

r_obj* r_exec_mask_n_call_poke(r_obj* fn_sym,
                               r_obj* fn,
                               const struct r_pair* args,
                               int n,
                               r_obj* env);

static inline
r_obj* r_exec_mask1(r_obj* fn_sym, r_obj* fn,
                    r_obj* x_sym, r_obj* x,
                    r_obj* env) {
  struct r_pair args[] = {
    { x_sym, x }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
r_obj* r_exec_mask2(r_obj* fn_sym, r_obj* fn,
                    r_obj* x1_sym, r_obj* x1,
                    r_obj* x2_sym, r_obj* x2,
                    r_obj* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
r_obj* r_exec_mask3(r_obj* fn_sym, r_obj* fn,
                    r_obj* x1_sym, r_obj* x1,
                    r_obj* x2_sym, r_obj* x2,
                    r_obj* x3_sym, r_obj* x3,
                    r_obj* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
r_obj* r_exec_mask4(r_obj* fn_sym, r_obj* fn,
                    r_obj* x1_sym, r_obj* x1,
                    r_obj* x2_sym, r_obj* x2,
                    r_obj* x3_sym, r_obj* x3,
                    r_obj* x4_sym, r_obj* x4,
                    r_obj* env) {
  struct r_pair args[] = {
    { x1_sym, x1 },
    { x2_sym, x2 },
    { x3_sym, x3 },
    { x4_sym, x4 }
  };
  return r_exec_mask_n(fn_sym, fn, args, R_ARR_SIZEOF(args), env);
}

static inline
r_obj* r_exec_mask5(r_obj* fn_sym, r_obj* fn,
                    r_obj* x1_sym, r_obj* x1,
                    r_obj* x2_sym, r_obj* x2,
                    r_obj* x3_sym, r_obj* x3,
                    r_obj* x4_sym, r_obj* x4,
                    r_obj* x5_sym, r_obj* x5,
                    r_obj* env) {
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
r_obj* r_exec_mask6(r_obj* fn_sym, r_obj* fn,
                    r_obj* x1_sym, r_obj* x1,
                    r_obj* x2_sym, r_obj* x2,
                    r_obj* x3_sym, r_obj* x3,
                    r_obj* x4_sym, r_obj* x4,
                    r_obj* x5_sym, r_obj* x5,
                    r_obj* x6_sym, r_obj* x6,
                    r_obj* env) {
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
r_obj* r_exec_mask7(r_obj* fn_sym, r_obj* fn,
                    r_obj* x1_sym, r_obj* x1,
                    r_obj* x2_sym, r_obj* x2,
                    r_obj* x3_sym, r_obj* x3,
                    r_obj* x4_sym, r_obj* x4,
                    r_obj* x5_sym, r_obj* x5,
                    r_obj* x6_sym, r_obj* x6,
                    r_obj* x7_sym, r_obj* x7,
                    r_obj* env) {
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


static inline
r_obj* r_lazy_eval(struct r_lazy lazy) {
  if (!lazy.env) {
    // Unitialised lazy variable
    return r_null;
  } else if (lazy.env == r_null) {
    // Forced lazy variable
    return lazy.x;
  } else {
    return r_eval(lazy.x, lazy.env);
  }
}

extern
struct r_lazy r_lazy_null;

extern
struct r_lazy r_lazy_missing_arg;

static inline
r_obj* r_lazy_eval_protect(struct r_lazy lazy) {
  r_obj* out = KEEP(r_lazy_eval(lazy));
  out = r_expr_protect(out);

  FREE(1);
  return out;
}

static inline
bool r_lazy_is_null(struct r_lazy call) {
  return !call.x && !call.env;
}

#endif
