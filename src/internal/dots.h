#ifndef RLANG_INTERNAL_DOTS_H
#define RLANG_INTERNAL_DOTS_H

#include <rlang.h>

bool is_splice_box(r_obj* x);
r_obj* rlang_unbox(r_obj* x);

enum dots_collect {
  DOTS_COLLECT_expr,
  DOTS_COLLECT_quo,
  DOTS_COLLECT_value
};
#define DOTS_COLLECT_MAX 3

enum dots_op {
  DOTS_OP_expr_none,
  DOTS_OP_expr_uq,
  DOTS_OP_expr_uqs,
  DOTS_OP_expr_uqn,
  DOTS_OP_expr_fixup,
  DOTS_OP_expr_dot_data,
  DOTS_OP_expr_curly,
  DOTS_OP_quo_none,
  DOTS_OP_quo_uq,
  DOTS_OP_quo_uqs,
  DOTS_OP_quo_uqn,
  DOTS_OP_quo_fixup,
  DOTS_OP_quo_dot_data,
  DOTS_OP_quo_curly,
  DOTS_OP_value_none,
  DOTS_OP_value_uq,
  DOTS_OP_value_uqs,
  DOTS_OP_value_uqn,
  DOTS_OP_value_fixup,
  DOTS_OP_value_dot_data,
  DOTS_OP_value_curly,
  DOTS_OP_MAX
};


#endif
