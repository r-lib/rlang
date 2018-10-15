#include <rlang.h>
#include "internal.h"


sexp* rlang_zap = NULL;

sexp* as_list_call = NULL;
sexp* as_list_s4_call = NULL;

void rlang_init_dots();
void rlang_init_expr_interp();
void rlang_init_eval_tidy();

void rlang_init_internal() {
  rlang_init_dots();
  rlang_init_expr_interp();
  rlang_init_eval_tidy();

  rlang_zap = rlang_ns_get("zap!");

  as_list_call = r_parse("as.list(x)");
  r_mark_precious(as_list_call);

  as_list_s4_call = r_parse("as(x, 'list')");
  r_mark_precious(as_list_s4_call);

  /* dots.c - enum dots_expansion_op */
  RLANG_ASSERT(OP_DOTS_MAX == DOTS_CAPTURE_TYPE_MAX * EXPANSION_OP_MAX);
}
