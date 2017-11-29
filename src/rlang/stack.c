#include "rlang.h"

void r_on_exit(sexp* expr, sexp* frame) {
  static sexp* on_exit_prim = NULL;
  if (!on_exit_prim) {
    on_exit_prim = r_base_ns_get("on.exit");
  }

  sexp* args = r_build_pairlist2(expr, r_scalar_lgl(1));
  sexp* lang = KEEP(r_build_call_node(on_exit_prim, args));

  r_eval(lang, frame);
  FREE(1);
}
