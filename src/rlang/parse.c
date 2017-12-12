#include "rlang.h"


enum r_operator r_which_operator(sexp* call) {
  if (r_typeof(call) != r_type_call) {
    r_abort("Internal error: Expected call to determine operator type");
  }

  sexp* head = r_node_car(call);
  if (r_typeof(head) != r_type_symbol) {
    return R_OP_NONE;
  }

  const char* name = r_sym_c_str(head);
  size_t len = strlen(name);
  bool is_unary = r_node_cddr(call) == r_null;

  switch (name[0]) {
  case 'w':
    if (strcmp(name, "while") == 0) {
      return R_OP_WHILE;
    } else {
      goto none;
    }
  case 'f':
    if (strcmp(name, "for") == 0) {
      return R_OP_FOR;
    } else {
      goto none;
    }
  case 'r':
    if (strcmp(name, "repeat") == 0) {
      return R_OP_REPEAT;
    } else {
      goto none;
    }
  case 'i':
    if (strcmp(name, "if") == 0) {
      return R_OP_IF;
    } else {
      goto none;
    }

  case '?':
    if (len == 1) {
      if (is_unary) {
        return R_OP_QUESTION_UNARY;
      } else {
        return R_OP_QUESTION;
      }
    } else {
      goto none;
    }

  case '<':
    switch (len) {
    case 1:
      return R_OP_LESS;
    case 2:
      switch (name[1]) {
      case '-': return R_OP_ASSIGN1;
      case '=': return R_OP_LESS_EQUAL;
      default: goto none;
      }
    case 3:
      if (name[1] == '<' && name[2] == '-') {
        return R_OP_ASSIGN2;
      } else {
        goto none;
      }
    default:
      goto none;
    }

  case '=':
    switch (len) {
    case 1:
      return R_OP_ASSIGN_EQUAL;
    case 2:
      if (name[1] == '=') {
        return R_OP_EQUAL;
      } else {
        goto none;
      }
    default:
      goto none;
    }

  case ':':
    switch (len) {
    case 1:
      return R_OP_COLON1;
    case 2:
      switch (name[1]) {
      case '=': return R_OP_COLON_EQUAL;
      case ':': return R_OP_COLON2;
      default: goto none;
      }
    case 3:
      if (name[1] == ':' && name[2] == ':') {
        return R_OP_COLON3;
      } else {
        goto none;
      }
    default: goto none;
    }

  case '~':
    if (len == 1) {
      if (is_unary) {
        return R_OP_TILDE_UNARY;
      } else {
        return R_OP_TILDE;
      }
    } else {
      goto none;
    }

  case '|':
    switch (len) {
    case 1:
      return R_OP_OR1;
    case 2:
      if (name[1] == '|') {
        return R_OP_OR2;
      } else {
        goto none;
      }
    default:
      goto none;
    }

  case '&':
    switch (len) {
    case 1:
      return R_OP_AND1;
    case 2:
      if (name[1] == '&') {
        return R_OP_AND2;
      } else {
        goto none;
      }
    default:
      goto none;
    }

  case '!':
    switch (len) {
    case 1: {
      sexp* arg = r_node_cadr(call);
      if (r_is_call(arg, "!")) {
        if (r_is_call(r_node_cadr(arg), "!")) {
          return R_OP_BANG3;
        } else {
          return R_OP_BANG2;
        }
      } else {
        return R_OP_BANG1;
      }
    }
    case 2:
      switch (name[1]) {
      case '!': return R_OP_BANG2;
      case '=': return R_OP_NOT_EQUAL;
      default: goto none;
      }
    case 3:
      if (name[1] == '!' && name[2] == '!') {
        return R_OP_BANG3;
      } else {
        goto none;
      }
    default:
      goto none;
    }

  case '+':
    if (len == 1) {
      if (is_unary) {
        return R_OP_PLUS_UNARY;
      } else {
        return R_OP_PLUS;
      }
    } else {
      goto none;
    }

  case '-':
    if (len == 1) {
      if (is_unary) {
        return R_OP_MINUS_UNARY;
      } else {
        return R_OP_MINUS;
      }
    } else {
      goto none;
    }

  case '*':
    if (len == 1) {
      return R_OP_TIMES;
    } else {
      goto none;
    }

  case '/':
    if (len == 1) {
      return R_OP_RATIO;
    } else {
      goto none;
    }

  case '%':
    switch (len) {
    case 1:
      goto none;
    case 2:
      if (name[1] == '%') {
        return R_OP_MODULO;
      } else {
        goto none;
      }
    default:
      if (name[len - 1] == '%') {
        return R_OP_SPECIAL;
      } else {
        goto none;
      }
    }

  case '^':
    if (len == 1) {
      return R_OP_HAT;
    } else {
      goto none;
    }
  case '$':
    if (len == 1) {
      return R_OP_DOLLAR;
    } else {
      goto none;
    }
  case '@':
    if (len == 1) {
      return R_OP_AT;
    } else {
      goto none;
    }
  case '(':
    if (len == 1) {
      return R_OP_PARENTHESES;
    } else {
      goto none;
    }

  case '[':
    switch (len) {
    case 1:
      return R_OP_BRACKETS1;
    case 2:
      if (name[1] == '[') {
        return R_OP_BRACKETS2;
      } else {
        goto none;
      }
    default:
      goto none;
    }

  case '{':
    if (len == 1) {
      return R_OP_BRACES;
    } else {
      goto none;
    }

  none:
  default:
    return R_OP_NONE;
  }
}
