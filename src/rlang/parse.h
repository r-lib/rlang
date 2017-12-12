#ifndef RLANG_PARSE_H
#define RLANG_PARSE_H


// This only includes operators that actually appear in the AST.
// Examples of silent operators are `else` and `in`.
enum r_operator {
  R_OP_NONE           = 0,
  R_OP_WHILE          = 1,
  R_OP_FOR            = 2,
  R_OP_REPEAT         = 3,
  R_OP_IF             = 4,
  R_OP_QUESTION       = 5,
  R_OP_QUESTION_UNARY = 6,
  R_OP_ASSIGN1        = 7,
  R_OP_ASSIGN2        = 8,
  R_OP_ASSIGN_EQUAL   = 9,
  R_OP_COLON_EQUAL    = 10,
  R_OP_TILDE          = 11,
  R_OP_TILDE_UNARY    = 12,
  R_OP_OR1            = 13,
  R_OP_OR2            = 14,
  R_OP_AND1           = 15,
  R_OP_AND2           = 16,
  R_OP_BANG1          = 17,
  R_OP_BANG3          = 18,
  R_OP_GREATER        = 19,
  R_OP_GREATER_EQUAL  = 20,
  R_OP_LESS           = 21,
  R_OP_LESS_EQUAL     = 22,
  R_OP_EQUAL          = 23,
  R_OP_NOT_EQUAL      = 24,
  R_OP_PLUS           = 25,
  R_OP_MINUS          = 26,
  R_OP_TIMES          = 27,
  R_OP_RATIO          = 28,
  R_OP_MODULO         = 29,
  R_OP_SPECIAL        = 30,
  R_OP_COLON1         = 31,
  R_OP_BANG2          = 32,
  R_OP_PLUS_UNARY     = 33,
  R_OP_MINUS_UNARY    = 34,
  R_OP_HAT            = 35,
  R_OP_DOLLAR         = 36,
  R_OP_AT             = 37,
  R_OP_COLON2         = 38,
  R_OP_COLON3         = 39,
  R_OP_PARENTHESES    = 40,
  R_OP_BRACKETS1      = 41,
  R_OP_BRACKETS2      = 42,
  R_OP_BRACES         = 43
};

enum r_operator r_which_operator(sexp* call);


#endif
