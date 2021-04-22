#include <rlang.h>

#include "internal.h"


r_obj* ffi_new_function(r_obj* args, r_obj* body, r_obj* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment");
  }

  args = KEEP(r_vec_coerce(args, R_TYPE_pairlist));

  r_obj* node = args;
  while (node != r_null) {
    if (r_node_tag(node) == r_null) {
      r_abort("All formal parameters in `args` must be named");
    }
    node = r_node_cdr(node);
  }

  r_obj* call = KEEP(r_call3(fns_function, args, body));
  r_obj* out = r_eval(call, env);

  FREE(2);
  return out;
}


static r_obj* as_function_call = NULL;

// TODO: Replace with C implementation of `as_function()`
r_obj* rlang_as_function(r_obj* x, r_obj* env) {
  return r_eval_with_xy(as_function_call, x, env, rlang_ns_env);
}

void rlang_init_fn() {
  as_function_call = r_parse("as_function(x, env = y)");
  r_preserve(as_function_call);
}
