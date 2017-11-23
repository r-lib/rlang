#ifndef RLANG_LANG_H
#define RLANG_LANG_H

#include "node.h"


static inline SEXP r_new_call_node(SEXP car, SEXP cdr) {
  return Rf_lcons(car, cdr);
}
static inline SEXP r_build_call_node(SEXP car, SEXP cdr) {
  SEXP out = KEEP(r_build_node(car, cdr));
  SET_TYPEOF(out, LANGSXP);
  FREE(1);
  return out;
}

static inline SEXP r_build_call(SEXP head) {
  return r_build_call_node(head, r_null);
}
static inline SEXP r_build_call1(SEXP head, SEXP arg1) {
  return r_build_call_node(head, r_build_pairlist(arg1));
}
static inline SEXP r_build_call2(SEXP head, SEXP arg1, SEXP arg2) {
  return r_build_call_node(head, r_build_pairlist2(arg1, arg2));
}

bool r_is_call(SEXP x, const char* name);
bool r_is_call_any(SEXP x, const char** names, int n);

bool r_is_prefixed_call(SEXP x, const char* name);
bool r_is_prefixed_call_any(SEXP x, const char ** names, int n);
bool r_is_maybe_prefixed_call_any(SEXP x, const char ** names, int n);

bool r_is_namespaced_call(SEXP x, const char* ns, const char* name);
bool r_is_namespaced_call_any(SEXP x, const char* ns, const char** names, int n);

bool r_is_special_op_call(SEXP x);


#endif
