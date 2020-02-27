#include "rlang.h"


sexp* r_eval_with_x(sexp* call, sexp* parent, sexp* x) {
  sexp* env = KEEP(r_new_environment(parent, 1));
  sexp* out = r_eval_in_with_x(call, env,
                               x, r_x_sym);

  FREE(1);
  return out;
}
sexp* r_eval_with_xy(sexp* call, sexp* parent, sexp* x, sexp* y) {
  sexp* env = KEEP(r_new_environment(parent, 1));
  sexp* out = r_eval_in_with_xy(call, env,
                                x, r_x_sym,
                                y, r_y_sym);

  FREE(1);
  return out;
}
sexp* r_eval_with_xyz(sexp* call, sexp* parent, sexp* x, sexp* y, sexp* z) {
  sexp* env = KEEP(r_new_environment(parent, 1));
  sexp* out = r_eval_in_with_xyz(call, env,
                                 x, r_x_sym,
                                 y, r_y_sym,
                                 z, r_z_sym);

  FREE(1);
  return out;
}
sexp* r_eval_with_wxyz(sexp* call, sexp* parent, sexp* w, sexp* x, sexp* y, sexp* z) {
  sexp* env = KEEP(r_new_environment(parent, 1));
  sexp* out = r_eval_in_with_wxyz(call, env,
                                  w, r_w_sym,
                                  x, r_x_sym,
                                  y, r_y_sym,
                                  z, r_z_sym);

  FREE(1);
  return out;
}

sexp* r_eval_in_with_x(sexp* call, sexp* env,
                       sexp* x, sexp* x_sym) {
  r_env_poke(env, x_sym, x);
  return r_eval(call, env);
}
sexp* r_eval_in_with_xy(sexp* call, sexp* env,
                        sexp* x, sexp* x_sym,
                        sexp* y, sexp* y_sym) {
  r_env_poke(env, x_sym, x);
  r_env_poke(env, y_sym, y);
  return r_eval(call, env);
}
sexp* r_eval_in_with_xyz(sexp* call, sexp* env,
                         sexp* x, sexp* x_sym,
                         sexp* y, sexp* y_sym,
                         sexp* z, sexp* z_sym) {
  r_env_poke(env, x_sym, x);
  r_env_poke(env, y_sym, y);
  r_env_poke(env, z_sym, z);
  return r_eval(call, env);
}
sexp* r_eval_in_with_wxyz(sexp* call, sexp* env,
                          sexp* w, sexp* w_sym,
                          sexp* x, sexp* x_sym,
                          sexp* y, sexp* y_sym,
                          sexp* z, sexp* z_sym) {
  r_env_poke(env, w_sym, w);
  r_env_poke(env, x_sym, x);
  r_env_poke(env, y_sym, y);
  r_env_poke(env, z_sym, z);
  return r_eval(call, env);
}

static sexp* shared_x_env;
static sexp* shared_xy_env;
static sexp* shared_xyz_env;

// Evaluate call with a preallocated environment containing a single
// `x` binding and inheriting from base env.
//
// Since this has side effects, it should not be used when there is a
// chance of recursing into the C library. It should only be used to
// evaluate pure R calls or functions from other packages, such as the
// base package.
sexp* eval_with_x(sexp* call, sexp* x) {
  r_env_poke(shared_x_env, r_x_sym, x);

  sexp* out = KEEP(r_eval(call, shared_x_env));

  // Release for gc
  r_env_poke(shared_x_env, r_x_sym, r_null);

  FREE(1);
  return out;
}

sexp* eval_with_xy(sexp* call, sexp* x, sexp* y) {
  r_env_poke(shared_xy_env, r_x_sym, x);
  r_env_poke(shared_xy_env, r_y_sym, y);

  sexp* out = KEEP(r_eval(call, shared_xy_env));

  // Release for gc
  r_env_poke(shared_xy_env, r_x_sym, r_null);
  r_env_poke(shared_xy_env, r_y_sym, r_null);

  FREE(1);
  return out;
}

sexp* eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z) {
  r_env_poke(shared_xyz_env, r_x_sym, x);
  r_env_poke(shared_xyz_env, r_y_sym, y);
  r_env_poke(shared_xyz_env, r_z_sym, z);

  sexp* out = KEEP(r_eval(call, shared_xyz_env));

  // Release for gc
  r_env_poke(shared_xyz_env, r_x_sym, r_null);
  r_env_poke(shared_xyz_env, r_y_sym, r_null);
  r_env_poke(shared_xyz_env, r_z_sym, r_null);

  FREE(1);
  return out;
}
