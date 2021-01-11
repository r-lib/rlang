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
#include "expr-interp.c"
#include "expr-interp-rotate.c"
#include "fn.c"
#include "hash.c"
#include "nse-defuse.c"
#include "parse.c"
#include "quo.c"
#include "replace-na.c"
#include "squash.c"
#include "sym-unescape.c"
#include "utils.c"
#include "vec-raw.c"
#include "weakref.c"


sexp* rlang_zap = NULL;
sexp* rlang_as_list_call = NULL;
sexp* rlang_objs_keep = NULL;
sexp* rlang_objs_trailing = NULL;
sexp* fns_function = NULL;
sexp* fns_quote = NULL;

void rlang_init_internal(sexp* ns) {
  rlang_init_utils();
  rlang_init_dots(ns);
  rlang_init_expr_interp();
  rlang_init_eval_tidy();
  rlang_init_attr(ns);

  rlang_zap = rlang_ns_get("zap!");

  rlang_as_list_call = r_parse("rlang_as_list(x)");
  r_mark_precious(rlang_as_list_call);


  rlang_objs_keep = r_chr("keep");
  r_mark_precious(rlang_objs_keep);

  rlang_objs_trailing = r_chr("trailing");
  r_mark_precious(rlang_objs_trailing);


  fns_function = r_eval(r_sym("function"), r_base_env);
  fns_quote = r_eval(r_sym("quote"), r_base_env);

  /* dots.c - enum dots_expansion_op */
  RLANG_ASSERT(OP_DOTS_MAX == DOTS_CAPTURE_TYPE_MAX * EXPANSION_OP_MAX);
}
