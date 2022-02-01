#ifndef RLANG_INTERNAL_AST_ROTATE_H
#define RLANG_INTERNAL_AST_ROTATE_H

#include "parse.h"


static inline
bool op_needs_fixup(enum r_operator op) {
  switch (op) {
  case R_OP_GREATER:
  case R_OP_GREATER_EQUAL:
  case R_OP_LESS:
  case R_OP_LESS_EQUAL:
  case R_OP_EQUAL:
  case R_OP_NOT_EQUAL:
  case R_OP_PLUS:
  case R_OP_MINUS:
  case R_OP_TIMES:
  case R_OP_RATIO:
  case R_OP_MODULO:
  case R_OP_SPECIAL:
  case R_OP_COLON1:
  case R_OP_PLUS_UNARY:
  case R_OP_MINUS_UNARY:
    return true;
  default:
    return false;
  }
}

static inline
bool is_problematic_op(r_obj* x) {
  return op_needs_fixup(r_which_operator(x));
}

r_obj* fixup_interp(r_obj* x, r_obj* env);
r_obj* fixup_interp_first(r_obj* x, r_obj* env);


#endif
