#include "rlang/rlang.h"

// cnd.c

SEXP rlang_cnd_signal(SEXP cnd, SEXP mufflable) {
  r_cnd_signal(cnd, r_as_bool(mufflable));
  return r_null;
}
SEXP rlang_cnd_inform(SEXP cnd, SEXP mufflable) {
  r_cnd_inform(cnd, r_as_bool(mufflable));
  return r_null;
}
SEXP rlang_cnd_warn(SEXP cnd, SEXP mufflable) {
  r_cnd_warn(cnd, r_as_bool(mufflable));
  return r_null;
}
SEXP rlang_cnd_abort(SEXP cnd, SEXP mufflable) {
  r_cnd_abort(cnd, r_as_bool(mufflable));
  return r_null;
}


// node.c

SEXP rlang_node_car(SEXP x) {
  return CAR(x);
}
SEXP rlang_node_cdr(SEXP x) {
  return CDR(x);
}
SEXP rlang_node_caar(SEXP x) {
  return CAAR(x);
}
SEXP rlang_node_cadr(SEXP x) {
  return CADR(x);
}
SEXP rlang_node_cdar(SEXP x) {
  return CDAR(x);
}
SEXP rlang_node_cddr(SEXP x) {
  return CDDR(x);
}
SEXP rlang_node_tail(SEXP x) {
  while (CDR(x) != r_null)
    x = CDR(x);
  return x;
}

SEXP rlang_node_poke_car(SEXP x, SEXP newcar) {
  SETCAR(x, newcar);
  return x;
}
SEXP rlang_node_poke_cdr(SEXP x, SEXP newcdr) {
  SETCDR(x, newcdr);
  return x;
}
SEXP rlang_node_poke_caar(SEXP x, SEXP newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
SEXP rlang_node_poke_cadr(SEXP x, SEXP newcar) {
  SETCADR(x, newcar);
  return x;
}
SEXP rlang_node_poke_cdar(SEXP x, SEXP newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
SEXP rlang_node_poke_cddr(SEXP x, SEXP newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

SEXP rlang_new_node_(SEXP car, SEXP cdr) {
  return Rf_cons(car, cdr);
}

SEXP rlang_node_tag(SEXP x) {
  return TAG(x);
}
SEXP rlang_node_poke_tag(SEXP x, SEXP tag) {
  SET_TAG(x, tag);
  return x;
}

SEXP rlang_on_exit(SEXP expr, SEXP frame) {
  r_on_exit(expr, frame);
  return r_null;
}


// lang.h

SEXP rlang_new_call_node(SEXP car, SEXP cdr) {
  return Rf_lcons(car, cdr);
}


// sexp.h

SEXP rlang_is_reference(SEXP x, SEXP y) {
  return r_scalar_lgl(x == y);
}

SEXP rlang_missing_arg() {
  return R_MissingArg;
}

SEXP rlang_duplicate(SEXP x, SEXP shallow) {
  return r_duplicate(x, r_as_bool(shallow));
}

SEXP rlang_is_null(SEXP x) {
  return r_scalar_lgl(r_is_null(x));
}

SEXP rlang_sxp_address(SEXP x) {
  static char str[1000];
  snprintf(str, 1000, "%p", (void*) x);
  return Rf_mkString(str);
}

SEXP rlang_poke_type(SEXP x, SEXP type) {
  SET_TYPEOF(x, Rf_str2type(r_c_string(type)));
  return x;
}
