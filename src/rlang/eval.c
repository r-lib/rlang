#include "rlang.h"


r_obj* r_eval_with_x(r_obj* call, r_obj* x, r_obj* parent) {
  r_obj* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.x, x);

  r_obj* out = r_eval(call, env);

  FREE(1);
  return out;
}
r_obj* r_eval_with_xy(r_obj* call, r_obj* x, r_obj* y, r_obj* parent) {
  r_obj* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.x, x);
  r_env_poke(env, r_syms.y, y);

  r_obj* out = r_eval(call, env);

  FREE(1);
  return out;
}
r_obj* r_eval_with_xyz(r_obj* call, r_obj* x, r_obj* y, r_obj* z, r_obj* parent) {
  r_obj* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.x, x);
  r_env_poke(env, r_syms.y, y);
  r_env_poke(env, r_syms.z, z);

  r_obj* out = r_eval(call, env);

  FREE(1);
  return out;
}
r_obj* r_eval_with_wxyz(r_obj* call, r_obj* w, r_obj* x, r_obj* y, r_obj* z, r_obj* parent) {
  r_obj* env = KEEP(r_alloc_environment(1, parent));
  r_env_poke(env, r_syms.w, w);
  r_env_poke(env, r_syms.x, x);
  r_env_poke(env, r_syms.y, y);
  r_env_poke(env, r_syms.z, z);

  r_obj* out = r_eval(call, env);

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

static r_obj* shared_x_env;
static r_obj* shared_xy_env;
static r_obj* shared_xyz_env;

r_obj* eval_with_x(r_obj* call, r_obj* x) {
  r_env_poke(shared_x_env, r_syms.x, x);

  r_obj* out = KEEP(r_eval(call, shared_x_env));

  // Release for gc
  r_env_poke(shared_x_env, r_syms.x, r_null);

  FREE(1);
  return out;
}

r_obj* eval_with_xy(r_obj* call, r_obj* x, r_obj* y) {
  r_env_poke(shared_xy_env, r_syms.x, x);
  r_env_poke(shared_xy_env, r_syms.y, y);

  r_obj* out = KEEP(r_eval(call, shared_xy_env));

  // Release for gc
  r_env_poke(shared_xy_env, r_syms.x, r_null);
  r_env_poke(shared_xy_env, r_syms.y, r_null);

  FREE(1);
  return out;
}

r_obj* eval_with_xyz(r_obj* call, r_obj* x, r_obj* y, r_obj* z) {
  r_env_poke(shared_xyz_env, r_syms.x, x);
  r_env_poke(shared_xyz_env, r_syms.y, y);
  r_env_poke(shared_xyz_env, r_syms.z, z);

  r_obj* out = KEEP(r_eval(call, shared_xyz_env));

  // Release for gc
  r_env_poke(shared_xyz_env, r_syms.x, r_null);
  r_env_poke(shared_xyz_env, r_syms.y, r_null);
  r_env_poke(shared_xyz_env, r_syms.z, r_null);

  FREE(1);
  return out;
}


r_obj* r_exec_mask_n(r_obj* fn_sym,
                     r_obj* fn,
                     const struct r_pair* args,
                     int n,
                     r_obj* parent) {
  r_obj* mask = KEEP(r_alloc_environment(n + 1, parent));
  r_obj* call = KEEP(r_exec_mask_n_call_poke(fn_sym, fn, args, n, mask));

  r_obj* out = r_eval(call, mask);

  FREE(2);
  return out;
}

r_obj* r_exec_n(r_obj* fn_sym,
                r_obj* fn,
                const struct r_pair* args,
                int n,
                r_obj* env) {
  r_obj* call = KEEP(r_exec_mask_n_call_poke(fn_sym, fn, args, n, env));
  r_obj* out = r_eval(call, env);

  FREE(1);
  return out;
}

// Create a call from arguments and poke elements with a non-NULL
// symbol in `env`. Symbolic arguments are protected from evaluation
// with `quote()`.
r_obj* r_exec_mask_n_call_poke(r_obj* fn_sym,
                               r_obj* fn,
                               const struct r_pair* args,
                               int n,
                               r_obj* env) {
  if (fn_sym != r_null) {
    r_env_poke(env, fn_sym, fn);
    fn = fn_sym;
  }

  r_obj* list = KEEP(r_new_pairlist(args, n, NULL));

  r_obj* node = list;
  while (node != r_null) {
    r_obj* car = r_node_car(node);
    r_obj* tag = r_node_tag(node);

    if (tag == r_null) {
      // If symbol is not supplied, protect symbolic arguments from
      // evaluation. If supplied this is not needed because of the
      // masking.
      r_node_poke_car(node, r_expr_protect(car));
    } else {
      // If symbol is supplied, assign the value in the environment and
      // use the symbol instead of the value in the list of arguments
      r_env_poke(env, tag, car);
      r_node_poke_car(node, tag);
    }

    node = r_node_cdr(node);
  }

  r_obj* call = r_new_call(fn, list);

  FREE(1);
  return call;
}


void r_init_library_eval() {
  r_lazy_missing_arg = (struct r_lazy) { .x = r_missing_arg, .env = r_null };
}

struct r_lazy r_lazy_null = { 0 };
struct r_lazy r_lazy_missing_arg = { 0 };
