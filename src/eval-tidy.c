#include "rlang/rlang.h"

sexp* rlang_ns_get(const char* name);


sexp* rlang_quo_eval(sexp* quo, sexp* frame) {
  static sexp* overscope_sym = NULL;
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
