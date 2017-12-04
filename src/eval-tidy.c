#include "rlang/rlang.h"

sexp* rlang_ns_get(const char* name);


static sexp* overscope_sym = NULL;

sexp* rlang_quo_eval(sexp* quo, sexp* frame) {
  if (!overscope_sym) overscope_sym = r_sym("_tidyeval_overscope");

  sexp* quo_expr = r_node_cadr(quo);
  if (quo_expr == r_missing_sym) {
    return r_missing_sym;
  }

  sexp* quo_env = r_get_attribute(quo, r_sym(".Environment"));
  sexp* overscope = r_env_find(frame, overscope_sym);

  if (overscope == r_unbound_sym) {
    return r_eval(quo_expr, quo_env);
  }

  static sexp* (quo_eval_overscoped_fn) = NULL;
  if (!quo_eval_overscoped_fn) {
    quo_eval_overscoped_fn = rlang_ns_get("quo_eval_overscoped");
  }

  // Prevent expression from being evaluated
  sexp* expr_wrapper = KEEP(r_expr_protect(quo_expr));

  sexp* call = KEEP(r_build_call3(quo_eval_overscoped_fn, expr_wrapper, quo_env, overscope));
  sexp* out = r_eval(call, r_empty_env);

  FREE(2);
  return out;
}

sexp* rlang_new_overscope(sexp* bottom, sexp* top, sexp* enclosure) {
  if (!overscope_sym) overscope_sym = r_sym("_tidyeval_overscope");

  static sexp* env_sym = NULL;
  static sexp* top_env_sym = NULL;
  if (!env_sym) {
    env_sym = r_sym(".env");
    top_env_sym = r_sym(".top_env");
  }

  if (top == r_null) {
    top = bottom;
  }

  // Create a child because we don't know what might be in bottom_env.
  // This way we can just remove all bindings between the parent of
  // `overscope` and `overscope_top`. We don't want to clean
  // everything in `overscope` in case the environment is leaked,
  // e.g. through a closure that might rely on some local bindings
  // installed by the user.
  sexp* overscope = KEEP(r_new_environment(bottom));

  r_env_poke(overscope, top_env_sym, top);
  r_env_poke(overscope, env_sym, enclosure);
  r_env_poke(overscope, overscope_sym, overscope);

  FREE(1);
  return overscope;
}
