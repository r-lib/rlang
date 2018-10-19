#include <rlang.h>


// attrs.c

sexp* rlang_poke_attributes(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}


// cnd.c

sexp* rlang_cnd_signal(sexp* cnd) {
  r_cnd_signal(cnd);
  return r_null;
}

sexp* rlang_cnd_type(sexp* cnd) {
  enum r_condition_type type = r_cnd_type(cnd);
  switch (type) {
  case r_cnd_type_condition: return r_chr("condition");
  case r_cnd_type_message: return r_chr("message");
  case r_cnd_type_warning: return r_chr("warning");
  case r_cnd_type_error: return r_chr("error");
  case r_cnd_type_interrupt: return r_chr("interrupt");
  default: r_abort("Internal error: Unhandled `r_condition_type`");
  }
}

sexp* rlang_interrupt() {
  r_interrupt();
  return r_null;
}

sexp* rlang_warn_deprecated_once(sexp* id, sexp* msg) {
  if (r_typeof(id) != r_type_character ||
      r_length(id) != 1 ||
      r_typeof(msg) != r_type_character ||
      r_length(msg) != 1) {
    r_abort("`id` and `msg` must be scalar strings");
  }
  r_warn_deprecated(r_chr_get_c_string(id, 0), r_chr_get_c_string(msg, 0));
  return r_null;
}


// env.c

sexp* rlang_env_poke_parent(sexp* env, sexp* new_parent) {
  if (R_IsNamespaceEnv(env)) {
    r_abort("Can't change the parent of a namespace environment");
  }
  if (R_IsPackageEnv(env)) {
    r_abort("Can't change the parent of a package environment");
  }
  if (R_EnvironmentIsLocked(env)) {
    r_abort("Can't change the parent of a locked environment");
  }
  if (env == r_global_env) {
    r_abort("Can't change the parent of the global environment");
  }
  if (env == r_base_env) {
    r_abort("Can't change the parent of the base environment");
  }
  if (env == r_empty_env) {
    r_abort("Can't change the parent of the empty environment");
  }

  SET_ENCLOS(env, new_parent);
  return env;
}

sexp* rlang_env_frame(sexp* env) {
  return FRAME(env);
}
sexp* rlang_env_hash_table(sexp* env) {
  return HASHTAB(env);
}


// eval.c

sexp* rlang_eval(sexp* expr, sexp* env) {
  return Rf_eval(expr, env);
}


// formula.c

sexp* rlang_is_formulaish(sexp* x, sexp* scoped, sexp* lhs) {
  int scoped_int = r_as_optional_bool(scoped);
  int lhs_int = r_as_optional_bool(lhs);

  bool out = r_is_formulaish(x, scoped_int, lhs_int);
  return r_lgl(out);
}


// parse.c

sexp* rlang_call_has_precedence(sexp* x, sexp* y, sexp* side) {
  bool has_predence;
  if (side == r_null) {
    has_predence = r_call_has_precedence(x, y);
  } else if (r_is_string(side, "lhs")) {
    has_predence = r_lhs_call_has_precedence(x, y);
  } else if (r_is_string(side, "rhs")) {
    has_predence = r_rhs_call_has_precedence(x, y);
  } else {
    r_abort("`side` must be NULL, \"lhs\" or \"rhs\"");
  }
  return r_lgl(has_predence);
}

sexp* rlang_which_operator(sexp* call) {
  const char* op = r_op_as_c_string(r_which_operator(call));
  return r_chr(op);
}


// node.c

sexp* rlang_node_car(sexp* x) {
  return CAR(x);
}
sexp* rlang_node_cdr(sexp* x) {
  return CDR(x);
}
sexp* rlang_node_caar(sexp* x) {
  return CAAR(x);
}
sexp* rlang_node_cadr(sexp* x) {
  return CADR(x);
}
sexp* rlang_node_cdar(sexp* x) {
  return CDAR(x);
}
sexp* rlang_node_cddr(sexp* x) {
  return CDDR(x);
}
sexp* rlang_node_tail(sexp* x) {
  while (CDR(x) != r_null)
    x = CDR(x);
  return x;
}

sexp* rlang_node_poke_car(sexp* x, sexp* newcar) {
  SETCAR(x, newcar);
  return x;
}
sexp* rlang_node_poke_cdr(sexp* x, sexp* newcdr) {
  SETCDR(x, newcdr);
  return x;
}
sexp* rlang_node_poke_caar(sexp* x, sexp* newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
sexp* rlang_node_poke_cadr(sexp* x, sexp* newcar) {
  SETCADR(x, newcar);
  return x;
}
sexp* rlang_node_poke_cdar(sexp* x, sexp* newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
sexp* rlang_node_poke_cddr(sexp* x, sexp* newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

sexp* rlang_new_node_(sexp* car, sexp* cdr) {
  return Rf_cons(car, cdr);
}

sexp* rlang_node_tag(sexp* x) {
  return TAG(x);
}
sexp* rlang_node_poke_tag(sexp* x, sexp* tag) {
  SET_TAG(x, tag);
  return x;
}

sexp* rlang_on_exit(sexp* expr, sexp* frame) {
  r_on_exit(expr, frame);
  return r_null;
}


// lang.h

sexp* rlang_new_call_node(sexp* car, sexp* cdr) {
  return Rf_lcons(car, cdr);
}


// quo.h

#include "../internal/quo.h"

sexp* rlang_quo_is_missing(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_missing(quo));
}
sexp* rlang_quo_is_symbol(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_symbol(quo));
}
sexp* rlang_quo_is_call(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_call(quo));
}
sexp* rlang_quo_is_symbolic(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_symbolic(quo));
}
sexp* rlang_quo_is_null(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_null(quo));
}


// sexp.h

sexp* rlang_length(sexp* x) {
  return r_int(r_length(x));
}
sexp* rlang_true_length(sexp* x) {
  return r_int(XTRUELENGTH(x));
}

sexp* rlang_is_reference(sexp* x, sexp* y) {
  return r_lgl(x == y);
}

sexp* rlang_missing_arg() {
  return R_MissingArg;
}

sexp* rlang_duplicate(sexp* x, sexp* shallow) {
  return r_duplicate(x, r_lgl_get(shallow, 0));
}

sexp* rlang_is_null(sexp* x) {
  return r_lgl(r_is_null(x));
}

sexp* rlang_sexp_address(sexp* x) {
  static char str[1000];
  snprintf(str, 1000, "%p", (void*) x);
  return Rf_mkString(str);
}

sexp* rlang_poke_type(sexp* x, sexp* type) {
  SET_TYPEOF(x, Rf_str2type(r_chr_get_c_string(type, 0)));
  return x;
}

sexp* rlang_mark_object(sexp* x) {
  SET_OBJECT(x, 1);
  return x;
}
sexp* rlang_unmark_object(sexp* x) {
  SET_OBJECT(x, 0);
  return x;
}

sexp* rlang_get_promise(sexp* x, sexp* env) {
  switch (r_typeof(x)) {
  case r_type_promise:
    return x;
  case r_type_character:
    if (r_length(x) == 1) {
      x = r_sym(r_chr_get_c_string(x, 0));
    } else {
      goto error;
    }
    // fallthrough
  case r_type_symbol: {
      sexp* prom = r_env_find(env, x);
      if (r_typeof(prom) == r_type_promise) {
        return prom;
      }
      // fallthrough
    }
  error:
  default:
    r_abort("`x` must be or refer to a local promise");
  }
}

sexp* rlang_promise_expr(sexp* x, sexp* env) {
  sexp* prom = rlang_get_promise(x, env);
  return PREXPR(prom);
}
sexp* rlang_promise_env(sexp* x, sexp* env) {
  sexp* prom = rlang_get_promise(x, env);
  return PRENV(prom);
}
sexp* rlang_promise_value(sexp* x, sexp* env) {
  sexp* prom = rlang_get_promise(x, env);
  sexp* value = PRVALUE(prom);
  if (value == r_unbound_sym) {
    return r_sym("R_UnboundValue");
  } else {
    return value;
  }
}

// vec.h

sexp* rlang_vec_coerce(sexp* x, sexp* type) {
  return Rf_coerceVector(x, Rf_str2type(r_chr_get_c_string(type, 0)));
}

// TODO: C-level check for scalar integerish
int r_as_int(sexp* x) {
  switch(r_typeof(x)) {
  case r_type_integer: return *INTEGER(x);
  case r_type_double: return (int) *REAL(x);
  default: r_abort("Internal error: Expected integerish input");
  }
}

sexp* rlang_vec_poke_n(sexp* x, sexp* offset,
                       sexp* y, sexp* from, sexp* n) {
  r_ssize offset_size = r_as_ssize(offset) - 1;
  r_ssize from_size = r_as_ssize(from) - 1;
  r_ssize n_size = r_as_ssize(n);

  r_vec_poke_n(x, offset_size, y, from_size, n_size);
  return x;
}

sexp* rlang_vec_poke_range(sexp* x, sexp* offset,
                           sexp* y, sexp* from, sexp* to) {
  r_ssize offset_size = r_as_ssize(offset) - 1;
  r_ssize from_size = r_as_ssize(from) - 1;
  r_ssize to_size = r_as_ssize(to) - 1;

  r_vec_poke_range(x, offset_size, y, from_size, to_size);
  return x;
}


// vec-list.h

static r_ssize validate_n(sexp* n) {
  if (n == r_null) {
    return -1;
  }

  switch (r_typeof(n)) {
  case r_type_integer:
  case r_type_double:
    if (r_length(n) == 1) {
      break;
    }
    // fallthrough
  default:
    r_abort("`n` must be NULL or a scalar integer");
  }

  return r_as_ssize(n);
}

static int validate_finite(sexp* finite) {
  switch (r_typeof(finite)) {
  case r_type_null:
    return -1;
  case r_type_integer:
  case r_type_double:
    finite = r_vec_coerce(finite, r_type_logical);
  case r_type_logical: {
    int value = r_lgl_get(finite, 0);
    if (value != NA_LOGICAL) {
      return r_lgl_get(finite, 0);
    } // else fallthrough
  }
  default:
    r_abort("`finite` must be NULL or a scalar logical");
  }
}

sexp* rlang_is_finite(sexp* x) {
  return r_shared_lgl(r_is_finite(x));
}

sexp* rlang_is_list(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_list(x, n));
}

sexp* rlang_is_atomic(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_atomic(x, n));
}
sexp* rlang_is_vector(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_vector(x, n));
}

sexp* rlang_is_logical(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_logical(x, n));
}
sexp* rlang_is_integer(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_integer(x, n, -1));
}
sexp* rlang_is_double(sexp* x, sexp* n_, sexp* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_double(x, n, finite));
}
sexp* rlang_is_integerish(sexp* x, sexp* n_, sexp* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_integerish(x, n, finite));
}

sexp* rlang_is_character(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_character(x, n));
}
sexp* rlang_is_raw(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_raw(x, n));
}
