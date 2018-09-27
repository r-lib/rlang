#include <rlang.h>
#include "internal.h"


sexp* as_list_call = NULL;

void rlang_init_dots();
void rlang_init_eval_tidy();

void rlang_init_internal() {
  rlang_init_dots();
  rlang_init_eval_tidy();

  as_list_call = r_parse("as.list(x)");
  r_mark_precious(as_list_call);

  /* dots.c - enum dots_expansion_op */
  RLANG_ASSERT(OP_DOTS_MAX == DOTS_CAPTURE_TYPE_MAX * EXPANSION_OP_MAX);
}
