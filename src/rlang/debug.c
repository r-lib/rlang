#include "rlang.h"


void r_sexp_inspect(sexp* x) {
  sexp* call = KEEP(r_parse(".Internal(inspect(x))"));
  r_eval_with_x(call, x, r_base_env);
  FREE(1);
}

void r_browse(sexp* x) {
  r_env_poke(r_global_env, r_sym(".debug"), x);

  r_printf("Object saved in `.debug`:\n");
  r_sexp_print(x);

  sexp* frame = KEEP(r_peek_frame());
  r_browse_at(frame);
  FREE(1);
}
void r_browse_at(sexp* env) {
  // The NULL expression is needed because of a limitation in ESS
  r_parse_eval("{ browser(); NULL }", env);
}

void r_dbg_str(sexp* x) {
  sexp* call = KEEP(r_parse("str(x)"));
  r_eval_with_x(call, x, r_ns_env("utils"));
  FREE(1);
}
