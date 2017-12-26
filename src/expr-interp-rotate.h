#ifndef RLANG_INTERNAL_EXPR_INTERP_FIXUP_H
#define RLANG_INTERNAL_EXPR_INTERP_FIXUP_H


static inline bool op_needs_fixup(enum r_operator op) {
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

static inline bool is_problematic_op(sexp* x) {
  return op_needs_fixup(r_which_operator(x));
}

sexp* fixup_interp(sexp* x, sexp* env);
sexp* fixup_interp_first(sexp* x, sexp* env);


#endif
