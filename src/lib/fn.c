#include "rlang.h"


sexp* r_new_function(sexp* formals, sexp* body, sexp* env) {
  sexp* args = KEEP(r_new_node(body, r_null));
  args = KEEP(r_new_node(formals, args));

  sexp* lang = KEEP(r_new_call(r_syms_function, args));
  sexp* fn = r_eval(lang, r_base_env);

  FREE(3);
  return fn;
}
