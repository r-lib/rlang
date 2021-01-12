#include "rlang.h"

static sexp* as_function_call = NULL;


sexp* r_new_function(sexp* formals, sexp* body, sexp* env) {
  sexp* args = KEEP(r_new_node(body, r_null));
  args = KEEP(r_new_node(formals, args));

  sexp* lang = KEEP(r_new_call(r_syms_function, args));
  sexp* fn = r_eval(lang, r_base_env);

  FREE(3);
  return fn;
}

// TODO: Replace with C implementation of `as_function()`
sexp* r_as_function(sexp* x, sexp* env) {
  return r_eval_with_xy(as_function_call, x, env, rlang_ns_env);
}


void r_init_library_fn() {
  as_function_call = r_parse("as_function(x, env = y)");
  r_mark_precious(as_function_call);
}
