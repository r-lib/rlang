#ifndef RLANG_PARSE_H
#define RLANG_PARSE_H


// This only includes operators that actually appear in the AST.
// Examples of silent operators are `else` and `in`.
enum r_operator {
  R_OP_NONE = 0,
  R_OP_WHILE,
  R_OP_FOR,
  R_OP_REPEAT,
  R_OP_IF,
  R_OP_QUESTION,
  R_OP_QUESTION_UNARY,
  R_OP_ASSIGN1,
  R_OP_ASSIGN2,
  R_OP_ASSIGN_EQUAL,
  R_OP_COLON_EQUAL,
  R_OP_TILDE,
  R_OP_TILDE_UNARY,
  R_OP_OR1,
  R_OP_OR2,
  R_OP_AND1,
  R_OP_AND2,
  R_OP_BANG1,
  R_OP_BANG3,
  R_OP_GREATER,
  R_OP_GREATER_EQUAL,
  R_OP_LESS,
  R_OP_LESS_EQUAL,
  R_OP_EQUAL,
  R_OP_NOT_EQUAL,
  R_OP_PLUS,
  R_OP_MINUS,
  R_OP_TIMES,
  R_OP_RATIO,
  R_OP_MODULO,
  R_OP_SPECIAL,
  R_OP_COLON1,
  R_OP_BANG2,
  R_OP_PLUS_UNARY,
  R_OP_MINUS_UNARY,
  R_OP_HAT,
  R_OP_DOLLAR,
  R_OP_AT,
  R_OP_COLON2,
  R_OP_COLON3,
  R_OP_PARENTHESES,
  R_OP_BRACKETS1,
  R_OP_BRACKETS2,
  R_OP_BRACES,
  R_OP_MAX
};

enum r_operator r_which_operator(sexp* call);


/**
 * @assoc
 *   -1 = left associative
 *    0 = non associative
 *    1 = right associative
 */
struct r_op_binding_power {
  uint8_t power;
  int8_t assoc;
  bool unary;
  bool delimited;
};

const struct r_op_binding_power r_ops_binding_powers[R_OP_MAX];
bool r_op_has_precedence(enum r_operator x, enum r_operator y);
bool r_call_has_precedence(sexp* x, sexp* y);


#endif
