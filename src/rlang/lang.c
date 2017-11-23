#include "rlang.h"

bool r_is_call(SEXP x, const char* name) {
  if (r_kind(x) != LANGSXP) {
    return false;
  } else {
    return name == NULL || r_is_symbol(r_node_car(x), name);
  }
}

bool r_is_call_any(SEXP x, const char** names, int n) {
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
  return r_is_call_any(head, r_subset_names, R_SUBSET_NAMES_N);
}

bool r_is_prefixed_call_any(SEXP x, const char ** names, int n) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  SEXP head = r_node_car(x);
  if (!r_is_call_any(head, r_subset_names, R_SUBSET_NAMES_N)) {
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

bool r_is_namespaced_call(SEXP x, const char* ns, const char* name) {
  if (r_kind(x) != LANGSXP) {
    return false;
  }

  SEXP head = r_node_car(x);
  if (!r_is_call(head, "::")) {
    return false;
  }

  if (ns) {
    SEXP lhs = r_node_cadr(head);
    if (!r_is_symbol(lhs, ns)) {
      return false;
    }
  }

  if (name) {
    SEXP rhs = r_node_cadr(r_node_cadr(x));
    if (!r_is_symbol(rhs, name)) {
      return false;
    }
  }

  return true;
}

bool r_is_namespaced_call_any(SEXP x, const char* ns,
                              const char** names, int n) {
  if (!r_is_namespaced_call(x, ns, NULL)) {
    return false;
  }

  SEXP args = r_node_cdar(x);
  SEXP sym = r_node_cadr(args);
  return r_is_symbol_any(sym, names, n);
}

bool r_is_special_op_call(SEXP x) {
  return
    r_kind(x) == LANGSXP &&
    r_is_special_op_sym(r_node_car(x));
}
