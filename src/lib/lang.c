#include "rlang.h"

bool r_is_call(sexp* x, const char* name) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  } else {
    return name == NULL || r_is_symbol(r_node_car(x), name);
  }
}

bool r_is_call_any(sexp* x, const char** names, int n) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  } else {
    return r_is_symbol_any(r_node_car(x), names, n);
  }
}


#define R_SUBSET_NAMES_N 4

static const char*
r_subset_names[R_SUBSET_NAMES_N] = { "$", "@", "::", ":::" };

bool r_is_prefixed_call(sexp* x, const char* name) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  }

  sexp* head = r_node_car(x);
  if (!r_is_call_any(head, r_subset_names, R_SUBSET_NAMES_N)) {
    return false;
  }

  if (name) {
    sexp* rhs = r_node_cadr(r_node_cdr(head));
    if (!r_is_symbol(rhs, name)) {
      return false;
    }
  }

  return true;
}

bool r_is_prefixed_call_any(sexp* x, const char ** names, int n) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  }

  sexp* head = r_node_car(x);
  if (!r_is_call_any(head, r_subset_names, R_SUBSET_NAMES_N)) {
    return false;
  }

  sexp* args = r_node_cdar(x);
  sexp* sym = r_node_cadr(args);
  return r_is_symbol_any(sym, names, n);
}

bool r_is_maybe_prefixed_call_any(sexp* x, const char ** names, int n) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  }

  if (r_is_symbol_any(r_node_car(x), names, n)) {
    return true;
  }

  return r_is_prefixed_call_any(x, names, n);
}

bool r_is_namespaced_call(sexp* x, const char* ns, const char* name) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  }

  sexp* head = r_node_car(x);
  if (!r_is_call(head, "::")) {
    return false;
  }

  if (ns) {
    sexp* lhs = r_node_cadr(head);
    if (!r_is_symbol(lhs, ns)) {
      return false;
    }
  }

  if (name) {
    sexp* rhs = r_node_cadr(r_node_cdar(x));
    if (!r_is_symbol(rhs, name)) {
      return false;
    }
  }

  return true;
}

bool r_is_namespaced_call_any(sexp* x, const char* ns,
                              const char** names, int n) {
  if (!r_is_namespaced_call(x, ns, NULL)) {
    return false;
  }

  sexp* args = r_node_cdar(x);
  sexp* sym = r_node_cadr(args);
  return r_is_symbol_any(sym, names, n);
}

bool r_is_special_op_call(sexp* x) {
  return
    r_typeof(x) == LANGSXP &&
    r_is_special_op_sym(r_node_car(x));
}

sexp* r_expr_protect(sexp* x) {
  static sexp* quote_prim = NULL;
  if (!quote_prim) quote_prim = r_base_ns_get("quote");

  sexp* args = KEEP(r_new_node(x, r_null));
  sexp* out = r_new_call(quote_prim, args);

  FREE(1);
  return out;
}
