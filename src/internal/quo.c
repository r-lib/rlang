#include <rlang.h>

static
const char* quo_tags[2] = { "quosure", "formula" };

r_obj* new_raw_formula(r_obj* lhs, r_obj* rhs, r_obj* env);

r_obj* ffi_new_quosure(r_obj* expr, r_obj* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment");
  }
  r_obj* quo = KEEP(new_raw_formula(r_null, expr, env));
  r_attrib_push_classes(quo, quo_tags, R_ARR_SIZEOF(quo_tags));
  FREE(1);
  return quo;
}
bool is_quosure(r_obj* x) {
  return r_typeof(x) == R_TYPE_call && r_inherits(x, "quosure");
}

inline void check_quosure(r_obj* quo) {
  if (!is_quosure(quo)) {
    r_abort("`quo` must be a quosure");
  }
}
r_obj* ffi_quo_get_expr(r_obj* quo) {
  check_quosure(quo);
  return r_node_cadr(quo);
}
r_obj* ffi_quo_set_expr(r_obj* quo, r_obj* expr) {
  check_quosure(quo);
  quo = r_clone(quo);
  r_node_poke_cadr(quo, expr);
  return quo;
}

r_obj* ffi_quo_get_env(r_obj* quo) {
  check_quosure(quo);
  return r_attrib_get(quo, r_syms.dot_environment);
}
r_obj* ffi_quo_set_env(r_obj* quo, r_obj* env) {
  check_quosure(quo);
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` must be an environment");
  }
  return r_attrib_set(quo, r_syms.dot_environment, env);
}

r_obj* ffi_get_expression(r_obj* x, r_obj* alternate) {
  switch (r_typeof(x)) {
  case LANGSXP:
    if (r_is_formula(x, -1, 0)) {
      return r_f_rhs(x);
    }
    break;
  // case CLOSXP:
  //   return r_fn_body(x);
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

bool quo_is_missing(r_obj* quo) {
  return r_node_cadr(quo) == R_MissingArg;
}
bool quo_is_symbol(r_obj* quo) {
  return r_typeof(r_node_cadr(quo)) == R_TYPE_symbol;
}
bool quo_is_call(r_obj* quo) {
  return r_typeof(r_node_cadr(quo)) == R_TYPE_call;
}
bool quo_is_symbolic(r_obj* quo) {
  return r_is_symbolic(r_node_cadr(quo));
}
bool quo_is_null(r_obj* quo) {
  return r_node_cadr(quo) == r_null;
}
