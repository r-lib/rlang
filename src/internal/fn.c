#include <rlang.h>

#include "internal.h"


sexp* rlang_new_function(sexp* args, sexp* body, sexp* env) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment");
  }

  args = KEEP(r_vec_coerce(args, r_type_pairlist));

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
