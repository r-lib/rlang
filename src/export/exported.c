#include <rlang.h>


// attrs.c

sexp* rlang_poke_attributes(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}


// cnd.c

sexp* rlang_cnd_signal(sexp* cnd, sexp* mufflable) {
  r_cnd_signal(cnd, r_as_bool(mufflable));
  return r_null;
}
sexp* rlang_cnd_inform(sexp* cnd, sexp* mufflable) {
  r_cnd_inform(cnd, r_as_bool(mufflable));
  return r_null;
}
sexp* rlang_cnd_warn(sexp* cnd, sexp* mufflable) {
  r_cnd_warn(cnd, r_as_bool(mufflable));
  return r_null;
}
sexp* rlang_cnd_abort(sexp* cnd, sexp* mufflable) {
  r_cnd_abort(cnd, r_as_bool(mufflable));
  return r_null;
}


// env.c

sexp* rlang_env_poke_parent(sexp* env, sexp* new_parent) {
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


// eval-tidy.c

sexp* rlang_new_dictionary(sexp* x, sexp* lookup_msg, sexp* read_only) {
  sexp* dict = KEEP(r_new_vector(VECSXP, 3));

  SET_VECTOR_ELT(dict, 0, x);
  SET_VECTOR_ELT(dict, 2, read_only);

  if (lookup_msg == r_null) {
    SET_VECTOR_ELT(dict, 1, Rf_mkString("Object `%s` not found in data"));
  } else {
    SET_VECTOR_ELT(dict, 1, lookup_msg);
  }

  static sexp* nms = NULL;
  if (!nms) {
    nms = r_new_vector(STRSXP, 3);
    R_PreserveObject(nms);
    SET_STRING_ELT(nms, 0, Rf_mkChar("src"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("lookup_msg"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("read_only"));
  }
  static sexp* s3 = NULL;
  if (!s3) {
    s3 = Rf_mkString("dictionary");
    R_PreserveObject(s3);
  }

  Rf_setAttrib(dict, R_ClassSymbol, s3);
  Rf_setAttrib(dict, R_NamesSymbol, nms);

  FREE(1);
  return dict;
}


// formula.c

sexp* rlang_is_formulaish(sexp* x, sexp* scoped, sexp* lhs) {
  int scoped_int = r_as_optional_bool(scoped);
  int lhs_int = r_as_optional_bool(lhs);

  bool out = r_is_formulaish(x, scoped_int, lhs_int);
  return Rf_ScalarLogical(out);
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

static void check_quosure(sexp* x) {
  if (r_typeof(x) != r_type_call || !r_inherits(x, "quosure")) {
    r_abort("Expected a quosure");
  }
}
sexp* rlang_quo_is_missing(sexp* quo) {
  check_quosure(quo);
  return r_scalar_lgl(r_node_cadr(quo) == r_missing_sym);
}
sexp* rlang_quo_is_symbol(sexp* quo) {
  check_quosure(quo);
  return r_scalar_lgl(r_typeof(r_node_cadr(quo)) == r_type_symbol);
}
sexp* rlang_quo_is_call(sexp* quo) {
  check_quosure(quo);
  return r_scalar_lgl(r_typeof(r_node_cadr(quo)) == r_type_call);
}
sexp* rlang_quo_is_symbolic(sexp* quo) {
  check_quosure(quo);
  return r_scalar_lgl(r_is_symbolic(r_node_cadr(quo)));
}
sexp* rlang_quo_is_null(sexp* quo) {
  check_quosure(quo);
  return r_scalar_lgl(r_node_cadr(quo) == r_null);
}


// sexp.h

sexp* rlang_length(sexp* x) {
  return Rf_ScalarInteger(r_length(x));
}
sexp* rlang_true_length(sexp* x) {
  return Rf_ScalarInteger(TRUELENGTH(x));
}

sexp* rlang_is_reference(sexp* x, sexp* y) {
  return r_scalar_lgl(x == y);
}

sexp* rlang_missing_arg() {
  return R_MissingArg;
}

sexp* rlang_duplicate(sexp* x, sexp* shallow) {
  return r_duplicate(x, r_as_bool(shallow));
}

sexp* rlang_is_null(sexp* x) {
  return r_scalar_lgl(r_is_null(x));
}

sexp* rlang_sxp_address(sexp* x) {
  static char str[1000];
  snprintf(str, 1000, "%p", (void*) x);
  return Rf_mkString(str);
}

sexp* rlang_poke_type(sexp* x, sexp* type) {
  SET_TYPEOF(x, Rf_str2type(r_c_string(type)));
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

// vec.h

sexp* rlang_vec_coerce(sexp* x, sexp* type) {
  return Rf_coerceVector(x, Rf_str2type(r_c_string(type)));
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
  r_size_t offset_size = r_as_int(offset) - 1;
  r_size_t from_size = r_as_int(from) - 1;
  r_size_t n_size = r_as_int(n);

  r_vec_poke_n(x, offset_size, y, from_size, n_size);
  return x;
}

sexp* rlang_vec_poke_range(sexp* x, sexp* offset,
                           sexp* y, sexp* from, sexp* to) {
  r_size_t offset_size = r_as_int(offset) - 1;
  r_size_t from_size = r_as_int(from) - 1;
  r_size_t to_size = r_as_int(to) - 1;

  r_vec_poke_range(x, offset_size, y, from_size, to_size);
  return x;
}
