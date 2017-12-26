#include "rlang.h"


const struct r_op_precedence r_ops_precedence[R_OP_MAX] = {
  [R_OP_NONE]           = { .power =   0,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_QUESTION]       = { .power =  10,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_QUESTION_UNARY] = { .power =  10,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_WHILE]          = { .power =  20,  .assoc = -1,  .unary = false,  .delimited =  true },
  [R_OP_FOR]            = { .power =  20,  .assoc = -1,  .unary = false,  .delimited =  true },
  [R_OP_REPEAT]         = { .power =  20,  .assoc = -1,  .unary = false,  .delimited =  true },
  [R_OP_IF]             = { .power =  30,  .assoc =  1,  .unary = false,  .delimited =  true },
  [R_OP_ASSIGN1]        = { .power =  40,  .assoc =  1,  .unary = false,  .delimited = false },
  [R_OP_ASSIGN2]        = { .power =  40,  .assoc =  1,  .unary = false,  .delimited = false },
  [R_OP_COLON_EQUAL]    = { .power =  40,  .assoc =  1,  .unary = false,  .delimited = false },
  [R_OP_ASSIGN_EQUAL]   = { .power =  50,  .assoc =  1,  .unary = false,  .delimited = false },
  [R_OP_TILDE]          = { .power =  60,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_TILDE_UNARY]    = { .power =  60,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_OR1]            = { .power =  70,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_OR2]            = { .power =  70,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_AND1]           = { .power =  80,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_AND2]           = { .power =  80,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_BANG1]          = { .power =  90,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_BANG3]          = { .power =  90,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_GREATER]        = { .power = 100,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_GREATER_EQUAL]  = { .power = 100,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_LESS]           = { .power = 100,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_LESS_EQUAL]     = { .power = 100,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_EQUAL]          = { .power = 100,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_NOT_EQUAL]      = { .power = 100,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_PLUS]           = { .power = 110,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_MINUS]          = { .power = 110,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_TIMES]          = { .power = 120,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_RATIO]          = { .power = 120,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_MODULO]         = { .power = 130,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_SPECIAL]        = { .power = 130,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_COLON1]         = { .power = 140,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_BANG2]          = { .power = 150,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_PLUS_UNARY]     = { .power = 150,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_MINUS_UNARY]    = { .power = 150,  .assoc = -1,  .unary =  true,  .delimited = false },
  [R_OP_HAT]            = { .power = 160,  .assoc =  1,  .unary = false,  .delimited = false },
  [R_OP_DOLLAR]         = { .power = 170,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_AT]             = { .power = 170,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_COLON2]         = { .power = 180,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_COLON3]         = { .power = 180,  .assoc =  0,  .unary = false,  .delimited = false },
  [R_OP_PARENTHESES]    = { .power = 190,  .assoc =  0,  .unary =  true,  .delimited =  true },
  [R_OP_BRACKETS1]      = { .power = 190,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_BRACKETS2]      = { .power = 190,  .assoc = -1,  .unary = false,  .delimited = false },
  [R_OP_BRACES]         = { .power = 200,  .assoc =  0,  .unary = false,  .delimited =  true }
};

enum r_operator r_which_operator(sexp* call) {
  if (r_typeof(call) != r_type_call) {
    return R_OP_NONE;
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

  case '>':
    switch (len) {
    case 1:
      return R_OP_GREATER;
    case 2:
      if (name[1] == '=') {
        return R_OP_GREATER_EQUAL;
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
    case 1:
      return R_OP_BANG1;
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
