#include "rlang.h"

void r_on_exit(SEXP expr, SEXP frame) {
  static SEXP on_exit_prim = NULL;
  if (!on_exit_prim) {
    on_exit_prim = base_obj("on.exit");
  }

  SEXP args = r_build_pairlist2(expr, r_scalar_lgl(1));
  SEXP lang = KEEP(r_build_call_node(on_exit_prim, args));

  r_eval(lang, frame);
  FREE(1);
}
