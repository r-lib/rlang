#include "rlang.h"


void r_sexp_inspect(sexp* x) {
  sexp* call = KEEP(r_parse(".Internal(inspect(x))"));
  r_eval_with_x(call, r_base_env, x);
  FREE(1);
}

void r_browse(sexp* env) {
  // The NULL expression is needed because of a limitation in ESS
  if (!env) {
    env = r_current_frame();
  }
  r_parse_eval("{ browser(); NULL }", env);
}
