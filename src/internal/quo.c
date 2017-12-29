#include <rlang.h>

#define QUO_TAGS_N 2
static const char* quo_tags[QUO_TAGS_N] = { "quosure", "formula" };

sexp* new_raw_formula(sexp* lhs, sexp* rhs, sexp* env);

sexp* r_new_quosure(sexp* expr, sexp* env) {
  sexp* quo = KEEP(new_raw_formula(r_null, expr, env));
  r_push_classes(quo, quo_tags, QUO_TAGS_N);
  FREE(1);
  return quo;
}

sexp* r_get_expression(sexp* x, sexp* alternate) {
  switch (r_typeof(x)) {
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
