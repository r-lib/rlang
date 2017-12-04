#include "rlang.h"

// From env.c
sexp* rlang_ns_get(const char* name);


sexp* r_new_quosure(sexp* expr, sexp* env) {
  static sexp* quo_eval_fn = NULL;
  if (!quo_eval_fn) {
    quo_eval_fn = rlang_ns_get("quo_eval");
  }

  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment");
  }

  sexp* args = KEEP(r_new_node_list(expr));
  sexp* quo = KEEP(r_new_call_node(quo_eval_fn, args));

  sexp* attrs = KEEP(r_new_node(env, r_null));
  r_node_poke_tag(attrs, r_sym(".Environment"));
  r_poke_attributes(quo, attrs);

  r_push_class(quo, "quosure");
  FREE(3);
  return quo;
}

sexp* r_get_expression(sexp* x, sexp* alternate) {
  switch (r_typeof(x)) {
  case LANGSXP:
    if (r_inherits(x, "quosure")) {
      return r_node_cadr(x);
    } else if (r_is_formulaish(x, -1, 0)) {
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
