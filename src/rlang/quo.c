#include "rlang.h"

#define QUO_TAGS_N 2
static const char* quo_tags[QUO_TAGS_N] = { "quosure", "formula" };

SEXP new_raw_formula(SEXP lhs, SEXP rhs, SEXP env);

SEXP r_new_quosure(SEXP expr, SEXP env) {
  SEXP quo = KEEP(new_raw_formula(r_null, expr, env));
  r_push_classes(quo, quo_tags, QUO_TAGS_N);
  FREE(1);
  return quo;
}

bool r_quo_is_missing(SEXP x) {
  return r_is_missing(r_f_rhs(x));
}

SEXP r_get_expression(SEXP x, SEXP alternate) {
  switch (r_kind(x)) {
  case LANGSXP:
    if (r_is_formulaish(x, -1, 0)) {
      return r_f_rhs(x);
    }
    break;
  case CLOSXP:
    return r_fn_body(x);
  case VECSXP:
    if (r_inherits(x, "frame")) {
      return r_list_get(x, 2);
    }
    break;
  default:
    break;
  }

  if (alternate) {
    return alternate;
  } else {
    return x;
  }
}
