#include "rlang.h"


void r_sexp_inspect(r_obj* x) {
  r_obj* call = KEEP(r_parse(".Internal(inspect(x))"));
  r_eval_with_x(call, x, r_envs.base);
  FREE(1);
}

void r_browse(r_obj* x) {
  r_env_poke(r_envs.global, r_sym(".debug"), x);

  r_printf("Object saved in `.debug`:\n");
  r_obj_print(x);

  r_obj* frame = KEEP(r_peek_frame());
  r_browse_at(frame);
  FREE(1);
}
void r_browse_at(r_obj* env) {
  // The NULL expression is needed because of a limitation in ESS
  r_parse_eval("{ browser(); NULL }", env);
}

void r_dbg_str(r_obj* x) {
  r_obj* call = KEEP(r_parse("str(x)"));
  r_eval_with_x(call, x, r_ns_env("utils"));
  FREE(1);
}
