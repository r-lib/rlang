#include "rlang.h"


r_obj* r_f_rhs(r_obj* f) {
  if (r_typeof(f) != LANGSXP) {
    r_abort("`x` must be a formula");
  }

  switch (r_length(f)) {
  case 2: return r_node_cadr(f);
  case 3: return CADDR(f);
  default: r_abort("Invalid formula");
  }
}
r_obj* r_f_lhs(r_obj* f) {
  if (r_typeof(f) != LANGSXP) {
    r_abort("`x` must be a formula");
  }

  switch (r_length(f)) {
  case 2: return r_null;
  case 3: return r_node_cadr(f);
  default: r_abort("Invalid formula");
  }
}
r_obj* r_f_env(r_obj* f) {
  return r_attrib_get(f, r_sym(".Environment"));
}

bool r_f_has_env(r_obj* f) {
  return r_is_environment(r_f_env(f));
}

bool r_is_formula(r_obj* x, int scoped, int lhs) {
  if (r_typeof(x) != R_TYPE_call) {
    return false;
  }

  if (r_node_car(x) != r_syms.tilde) {
    return false;
  }

  if (scoped >= 0) {
    bool has_env = r_typeof(r_f_env(x)) == R_TYPE_environment;
    bool has_class = r_inherits(x, "formula");
    if (scoped != (has_env && has_class)) {
      return false;
    }
  }

  if (lhs >= 0) {
    int has_lhs = r_length(x) > 2;
    if (lhs != has_lhs) {
      return false;
    }
  }

  return true;
}

r_obj* new_raw_formula(r_obj* lhs, r_obj* rhs, r_obj* env) {
  static r_obj* tilde_sym = NULL;
  if (!tilde_sym) {
    tilde_sym = r_sym("~");
  }
  if (!r_is_environment(env) && env != r_null) {
    r_abort("`env` must be an environment");
  }

  r_obj* f;
  r_obj* args;
  if (lhs == r_null) {
    args = KEEP(r_pairlist(rhs));
  } else {
    args = KEEP(r_pairlist2(lhs, rhs));
  }
  f = KEEP(r_new_call(tilde_sym, args));

  r_obj* attrs = KEEP(r_new_node(env, r_null));
  r_node_poke_tag(attrs, r_sym(".Environment"));
  r_poke_attrib(f, attrs);

  FREE(3);
  return f;
}
r_obj* r_new_formula(r_obj* lhs, r_obj* rhs, r_obj* env) {
  r_obj* f = KEEP(new_raw_formula(lhs, rhs, env));
  r_attrib_push_class(f, "formula");

  FREE(1);
  return f;
}
