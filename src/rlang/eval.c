#include "rlang.h"


sexp* r_eval_with_x(sexp* call, sexp* x, sexp* parent) {
  sexp* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.x, x);

  sexp* out = r_eval(call, env);

  FREE(1);
  return out;
}
sexp* r_eval_with_xy(sexp* call, sexp* x, sexp* y, sexp* parent) {
  sexp* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.x, x);
  r_env_poke(env, r_syms.y, y);

  sexp* out = r_eval(call, env);

  FREE(1);
  return out;
}
sexp* r_eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z, sexp* parent) {
  sexp* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.x, x);
  r_env_poke(env, r_syms.y, y);
  r_env_poke(env, r_syms.z, z);

  sexp* out = r_eval(call, env);

  FREE(1);
  return out;
}
sexp* r_eval_with_wxyz(sexp* call, sexp* w, sexp* x, sexp* y, sexp* z, sexp* parent) {
  sexp* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.w, w);
  r_env_poke(env, r_syms.x, x);
  r_env_poke(env, r_syms.y, y);
  r_env_poke(env, r_syms.z, z);

  sexp* out = r_eval(call, env);

  FREE(1);
  return out;
}


// Evaluate call with a preallocated environment containing a single
// `x` binding and inheriting from base env.
//
// Since this has side effects, it should not be used when there is a
// chance of recursing into the C library. It should only be used to
// evaluate pure R calls or functions from other packages, such as the
// base package.

static sexp* shared_x_env;
static sexp* shared_xy_env;
static sexp* shared_xyz_env;

sexp* eval_with_x(sexp* call, sexp* x) {
  r_env_poke(shared_x_env, r_syms.x, x);

  sexp* out = KEEP(r_eval(call, shared_x_env));

  // Release for gc
  r_env_poke(shared_x_env, r_syms.x, r_null);

  FREE(1);
  return out;
}

sexp* eval_with_xy(sexp* call, sexp* x, sexp* y) {
  r_env_poke(shared_xy_env, r_syms.x, x);
  r_env_poke(shared_xy_env, r_syms.y, y);

  sexp* out = KEEP(r_eval(call, shared_xy_env));

  // Release for gc
  r_env_poke(shared_xy_env, r_syms.x, r_null);
  r_env_poke(shared_xy_env, r_syms.y, r_null);

  FREE(1);
  return out;
}

sexp* eval_with_xyz(sexp* call, sexp* x, sexp* y, sexp* z) {
  r_env_poke(shared_xyz_env, r_syms.x, x);
  r_env_poke(shared_xyz_env, r_syms.y, y);
  r_env_poke(shared_xyz_env, r_syms.z, z);

  sexp* out = KEEP(r_eval(call, shared_xyz_env));

  // Release for gc
  r_env_poke(shared_xyz_env, r_syms.x, r_null);
  r_env_poke(shared_xyz_env, r_syms.y, r_null);
  r_env_poke(shared_xyz_env, r_syms.z, r_null);

  FREE(1);
  return out;
}


sexp* r_exec_mask_n(sexp* fn_sym,
                    sexp* fn,
                    const struct r_pair* args,
                    int n,
                    sexp* parent) {
  sexp* mask = KEEP(r_alloc_environment(n + 1, parent));
  sexp* call = KEEP(r_exec_mask_n_call_poke(fn_sym, fn, args, n, mask));

  sexp* out = r_eval(call, mask);

  FREE(2);
  return out;
}

// Create a call from arguments and poke elements with a non-NULL
// symbol in `env`
sexp* r_exec_mask_n_call_poke(sexp* fn_sym,
                              sexp* fn,
                              const struct r_pair* args,
                              int n,
                              sexp* env) {
  if (fn_sym != r_null) {
    r_env_poke(env, fn_sym, fn);
    fn = fn_sym;
  }

  sexp* shelter = KEEP(r_new_node(R_NilValue, R_NilValue));
  sexp* node = shelter;

  for (int i = 0; i < n; ++i) {
    struct r_pair arg = args[i];
    sexp* tag = arg.x;
    sexp* car = arg.y;

    if (tag != r_null) {
      r_env_poke(env, tag, car);
      car = tag;
    }

    sexp* cdr = r_new_node(car, r_null);
    r_node_poke_tag(cdr, tag);

    r_node_poke_cdr(node, cdr);
    node = cdr;
  }

  sexp* call = r_new_call(fn, r_node_cdr(shelter));

  FREE(1);
  return call;
}
