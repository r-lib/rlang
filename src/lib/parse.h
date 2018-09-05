#ifndef RLANG_PARSE_H
#define RLANG_PARSE_H

sexp* r_parse(const char* str);
sexp* r_parse_eval(const char* str, sexp* env);

// This only includes operators that actually appear in the AST.
// Examples of silent operators are `else` and `in`.
enum r_operator {
  R_OP_NONE = 0,
  R_OP_FUNCTION,
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
const char* r_op_as_c_string(enum r_operator op);


/**
 * struct r_op_precedence - Information about operator precedence
 *
 * @power: Binding power. Absolute value has no meaning, only the
 *   relative ordering between operators has meaning.
 * @assoc: -1 if left associative, 0 if non-associative, 1 if right associative.
 * @unary: `false` if a binary operation.
 * @delimited: `true` if an operation like `(` or `{`.
 */
struct r_op_precedence {
  uint8_t power;
  int8_t assoc;
  bool unary;
  bool delimited;
};

extern const struct r_op_precedence r_ops_precedence[R_OP_MAX];

/**
 * r_op_has_precedence() - Does an operation have precedence over another?
 *
 * Relies on information in the table of operation metadata
 * %r_ops_precedence.
 *
 * @x The call that was found lower in the AST (i.e. the call that is
 *   supposed to have precedence).
 * @parent The call that was found earlier in the AST (i.e. the one
 *   that wraps @x).
 */
bool r_op_has_precedence(enum r_operator x, enum r_operator parent);
bool r_rhs_op_has_precedence(enum r_operator rhs, enum r_operator parent);
bool r_lhs_op_has_precedence(enum r_operator lhs, enum r_operator parent);

static inline bool r_call_has_precedence(sexp* x, sexp* parent) {
  return r_op_has_precedence(r_which_operator(x), r_which_operator(parent));
}
static inline bool r_lhs_call_has_precedence(sexp* lhs, sexp* parent) {
  return r_lhs_op_has_precedence(r_which_operator(lhs), r_which_operator(parent));
}
static inline bool r_rhs_call_has_precedence(sexp* rhs, sexp* parent) {
  return r_rhs_op_has_precedence(r_which_operator(rhs), r_which_operator(parent));
}


#endif
