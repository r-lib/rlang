#include "rlang.h"


void r_sexp_inspect(sexp* x) {
  sexp* call = KEEP(r_parse(".Internal(inspect(x))"));
  r_eval_with_x(call, r_base_env, x);
  FREE(1);
}

void r_browse(SEXP x) {
  r_env_poke(R_GlobalEnv, Rf_install(".debug"), x);

  Rprintf("Object saved in `.debug`:\n");
  Rf_PrintValue(x);

  r_browse_at(KEEP(r_current_frame())); FREE(1);
}
void r_browse_at(sexp* env) {
  // The NULL expression is needed because of a limitation in ESS
  r_parse_eval("{ browser(); NULL }", env);
}
