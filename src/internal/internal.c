#include <rlang.h>
#include "internal.h"


sexp* rlang_zap = NULL;

sexp* as_list_call = NULL;
sexp* as_list_s4_call = NULL;

sexp* rlang_objs_keep = NULL;
sexp* rlang_objs_trailing = NULL;

sexp* fns_function = NULL;
sexp* fns_quote = NULL;

void rlang_init_utils();
void rlang_init_dots();
void rlang_init_expr_interp();
void rlang_init_eval_tidy();

void rlang_init_internal(sexp* ns) {
  rlang_init_utils();
  rlang_init_dots(ns);
  rlang_init_expr_interp();
  rlang_init_eval_tidy();

  rlang_zap = rlang_ns_get("zap!");

  as_list_call = r_parse("as.list(x)");
  r_mark_precious(as_list_call);

  as_list_s4_call = r_parse("as(x, 'list')");
  r_mark_precious(as_list_s4_call);


  rlang_objs_keep = r_chr("keep");
  r_mark_precious(rlang_objs_keep);

  rlang_objs_trailing = r_chr("trailing");
  r_mark_precious(rlang_objs_trailing);


  fns_function = r_eval(r_sym("function"), r_base_env);
  fns_quote = r_eval(r_sym("quote"), r_base_env);

  /* dots.c - enum dots_expansion_op */
  RLANG_ASSERT(OP_DOTS_MAX == DOTS_CAPTURE_TYPE_MAX * EXPANSION_OP_MAX);
}
