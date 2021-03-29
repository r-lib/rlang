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


sexp* rlang_formula_formals = NULL;

// [[ register_callable() ]]
sexp* as_function(sexp* x, const char* arg) {
  switch (r_typeof(x)) {
  case R_TYPE_closure:
  case R_TYPE_builtin:
  case R_TYPE_special:
    return x;

  case R_TYPE_call:
    if (r_node_car(x) == r_syms.tilde && r_node_cddr(x) == r_null) {
      sexp* env = r_attrib_get(x, r_syms.dot_environment);
      if (env == r_null) {
        r_abort("Can't transform formula to function because it doesn't have an environment.");
      }

      return r_new_function(rlang_formula_formals, r_node_cadr(x), env);
    }
    // else fallthrough;
  default:
    r_abort("Can't convert `%s` to a function", arg);
  }
}

void rlang_init_fn(sexp* ns) {
  const char* formals_code = "pairlist2(... = , .x = quote(..1), .y = quote(..2), . = quote(..1))";
  rlang_formula_formals = r_parse_eval(formals_code, ns);
  r_preserve_global(rlang_formula_formals);
}
