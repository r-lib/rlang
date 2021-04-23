#include <rlang.h>
#include "internal.h"


#include "arg.c"
#include "attr.c"
#include "call.c"
#include "dots.c"
#include "env.c"
#include "env-binding.c"
#include "eval.c"
#include "eval-tidy.c"
#include "exported.c"
#include "exported-tests.c"
#include "nse-inject.c"
#include "ast-rotate.c"
#include "fn.c"
#include "hash.c"
#include "nse-defuse.c"
#include "parse.c"
#include "quo.c"
#include "replace-na.c"
#include "squash.c"
#include "sym-unescape.c"
#include "utils.c"
#include "vec.c"
#include "vec-raw.c"
#include "weakref.c"
#include "init.c"


r_obj* rlang_zap = NULL;
r_obj* rlang_as_list_call = NULL;
r_obj* rlang_objs_keep = NULL;
r_obj* rlang_objs_trailing = NULL;
r_obj* fns_function = NULL;
r_obj* fns_quote = NULL;

void rlang_init_arg(r_obj* ns);

void rlang_init_internal(r_obj* ns) {
  rlang_init_utils();
  rlang_init_arg(ns);
  rlang_init_attr(ns);
  rlang_init_dots(ns);
  rlang_init_expr_interp();
  rlang_init_eval_tidy();
  rlang_init_fn();

  rlang_zap = rlang_ns_get("zap!");

  rlang_as_list_call = r_parse("rlang_as_list(x)");
  r_preserve(rlang_as_list_call);


  rlang_objs_keep = r_chr("keep");
  r_preserve(rlang_objs_keep);

  rlang_objs_trailing = r_chr("trailing");
  r_preserve(rlang_objs_trailing);


  fns_function = r_eval(r_sym("function"), r_envs.base);
  fns_quote = r_eval(r_sym("quote"), r_envs.base);

  /* dots.c - enum dots_op */
  RLANG_ASSERT(DOTS_OP_MAX == DOTS_COLLECT_MAX * INJECTION_OP_MAX);
}
