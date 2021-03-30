#include <rlang.h>

#include "internal.h"


sexp* rlang_new_function(sexp* args, sexp* body, sexp* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment");
  }

  args = KEEP(r_vec_coerce(args, R_TYPE_pairlist));

  sexp* node = args;
  while (node != r_null) {
    if (r_node_tag(node) == r_null) {
      r_abort("All formal parameters in `args` must be named");
    }
    node = r_node_cdr(node);
  }

  sexp* call = KEEP(r_call3(fns_function, args, body));
  sexp* out = r_eval(call, env);

  FREE(2);
  return out;
}


static sexp* as_function_call = NULL;

// TODO: Replace with C implementation of `as_function()`
sexp* rlang_as_function(sexp* x, sexp* env) {
  return r_eval_with_xy(as_function_call, x, env, rlang_ns_env);
}

void rlang_init_fn() {
  as_function_call = r_parse("as_function(x, env = y)");
  r_preserve(as_function_call);
}
