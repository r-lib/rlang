#include "rlang.h"

SEXP r_new_language_(SEXP head, SEXP tail) {
  return Rf_lcons(head, tail);
}
SEXP r_new_language(SEXP head, SEXP tail) {
  KEEP(head);
  KEEP(tail);
  SEXP out = Rf_lcons(head, tail);
  FREE(2);
  return out;
}

bool r_is_language(SEXP x, const char* name) {
  if (r_kind(x) != LANGSXP) {
    return false;
  } else {
    return name == NULL || r_is_symbol(r_node_car(x), name);
  }
}

bool r_is_language_any(SEXP x, const char** names, int n) {
  if (r_kind(x) != LANGSXP) {
    return false;
  } else {
    return r_is_symbol_any(r_node_car(x), names, n);
  }
}


#define R_SUBSET_NAMES_N 4

static const char*
r_subset_names[R_SUBSET_NAMES_N] = { "$", "@", "::", ":::" };

bool r_is_prefixed_call(SEXP x) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  SEXP head = r_node_car(x);
  return r_is_language_any(head, r_subset_names, R_SUBSET_NAMES_N);
}

bool r_is_prefixed_call_any(SEXP x, const char ** names, int n) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  SEXP head = r_node_car(x);
  if (!r_is_language_any(head, r_subset_names, R_SUBSET_NAMES_N)) {
    return false;
  }

  SEXP args = r_node_cdar(x);
  SEXP sym = r_node_cadr(args);
  return r_is_symbol_any(sym, names, n);
}

bool r_is_maybe_prefixed_call_any(SEXP x, const char ** names, int n) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  if (r_is_symbol_any(r_node_car(x), names, n)) {
    return true;
  }

  return r_is_prefixed_call_any(x, names, n);
}

bool r_is_namespaced_call(SEXP x, const char* ns) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  SEXP head = r_node_car(x);
  return r_is_language(head, "::") && r_is_symbol(r_node_cadr(head), ns);
}

bool r_is_namespaced_call_any(SEXP x, const char* ns,
                              const char** names, int n) {
  if (!r_is_namespaced_call(x, ns)) {
    return false;
  }

  SEXP args = r_node_cdar(x);
  SEXP sym = r_node_cadr(args);
  return r_is_symbol_any(sym, names, n);
}
