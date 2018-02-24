#include "rlang.h"


void r_sexp_inspect(sexp* x, sexp* env) {
  sexp* inspect_args = KEEP(r_new_node(x, r_null));
  sexp* inspect_call = KEEP(r_new_call(r_sym("inspect"), inspect_args));

  sexp* internal = r_base_ns_get(".I""nternal");
  sexp* internal_args = KEEP(r_new_node(inspect_call, r_null));
  sexp* internal_call = KEEP(r_new_call(internal, internal_args));
  r_eval(internal_call, env);

  FREE(4);
}
