#include "rlang.h"


r_obj* rlang_formula_formals = NULL;

r_obj* r_as_function(r_obj* x, const char* arg) {
  switch (r_typeof(x)) {
  case R_TYPE_closure:
  case R_TYPE_builtin:
  case R_TYPE_special:
    return x;

  case R_TYPE_call:
    if (r_node_car(x) == r_syms.tilde && r_node_cddr(x) == r_null) {
      r_obj* env = r_attrib_get(x, r_syms.dot_environment);
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

void r_init_library_fn() {
  const char* formals_code = "formals(function(..., .x = ..1, .y = ..2, . = ..1) NULL)";
  rlang_formula_formals = r_parse_eval(formals_code, r_envs.base);
  r_preserve_global(rlang_formula_formals);
}
