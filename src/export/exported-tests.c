#include <rlang.h>

sexp* rlang_test_r_warn(sexp* x) {
  r_warn(CHAR(STRING_ELT(x, 0)));
  return r_null;
}

sexp* rlang_r_string(sexp* str) {
  return STRING_ELT(str, 0);
}


// env.c

sexp* rlang_test_base_ns_get(sexp* name) {
  return r_base_ns_get(r_c_string(name));
}


// parse.c

bool call_has_precedence(sexp* x, sexp* y);
sexp* rlang_test_call_has_precedence(sexp* x, sexp* y) {
  return r_scalar_lgl(call_has_precedence(x, y));
}

sexp* rlang_test_which_operator(sexp* call) {
  enum r_operator op_key = r_which_operator(call);
  const char* op;

  switch (op_key) {
  case R_OP_NONE:           op = ""; break;
  case R_OP_WHILE:          op = "while"; break;
  case R_OP_FOR:            op = "for"; break;
  case R_OP_REPEAT:         op = "repeat"; break;
  case R_OP_IF:             op = "if"; break;
  case R_OP_QUESTION:       op = "?"; break;
  case R_OP_QUESTION_UNARY: op = "?unary"; break;
  case R_OP_ASSIGN1:        op = "<-"; break;
  case R_OP_ASSIGN2:        op = "<<-"; break;
  case R_OP_ASSIGN_EQUAL:   op = "="; break;
  case R_OP_COLON_EQUAL:    op = ":="; break;
  case R_OP_TILDE:          op = "~"; break;
  case R_OP_TILDE_UNARY:    op = "~unary"; break;
  case R_OP_OR1:            op = "|"; break;
  case R_OP_OR2:            op = "||"; break;
  case R_OP_AND1:           op = "&"; break;
  case R_OP_AND2:           op = "&&"; break;
  case R_OP_BANG1:          op = "!"; break;
  case R_OP_BANG3:          op = "!!!"; break;
  case R_OP_GREATER:        op = ">"; break;
  case R_OP_GREATER_EQUAL:  op = ">="; break;
  case R_OP_LESS:           op = "<"; break;
  case R_OP_LESS_EQUAL:     op = "<="; break;
  case R_OP_EQUAL:          op = "=="; break;
  case R_OP_NOT_EQUAL:      op = "!="; break;
  case R_OP_PLUS:           op = "+"; break;
  case R_OP_MINUS:          op = "-"; break;
  case R_OP_TIMES:          op = "*"; break;
  case R_OP_RATIO:          op = "/"; break;
  case R_OP_MODULO:         op = "%%"; break;
  case R_OP_SPECIAL:        op = "special"; break;
  case R_OP_COLON1:         op = ":"; break;
  case R_OP_BANG2:          op = "!!"; break;
  case R_OP_PLUS_UNARY:     op = "+unary"; break;
  case R_OP_MINUS_UNARY:    op = "-unary"; break;
  case R_OP_HAT:            op = "^"; break;
  case R_OP_DOLLAR:         op = "$"; break;
  case R_OP_AT:             op = "@"; break;
  case R_OP_COLON2:         op = "::"; break;
  case R_OP_COLON3:         op = ":::"; break;
  case R_OP_PARENTHESES:    op = "("; break;
  case R_OP_BRACKETS1:      op = "["; break;
  case R_OP_BRACKETS2:      op = "[["; break;
  case R_OP_BRACES:         op = "{"; break;
  case R_OP_MAX:            r_abort("Unexpected `enum r_operator` value");
  }

  return r_scalar_chr(op);
}


// sym.c

sexp* rlang_test_is_special_op_sym(sexp* x) {
  return Rf_ScalarLogical(r_is_special_op_sym(x));
}


// squash.c

bool rlang_is_clevel_spliceable(sexp* x) {
  return Rf_inherits(x, "foo");
}


// stack.c

sexp* rlang_test_sys_call(sexp* n) {
  return r_sys_call(r_c_int(n), NULL);
}
sexp* rlang_test_sys_frame(sexp* n) {
  return r_sys_frame(r_c_int(n), NULL);
}
