#include "rlang/rlang.h"


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

sexp* rlang_mut_env_parent(sexp* env, sexp* new_parent) {
  SET_ENCLOS(env, new_parent);
  return env;
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


// sexp.h

sexp* rlang_length(sexp* x) {
  return Rf_ScalarInteger(r_length(x));
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
