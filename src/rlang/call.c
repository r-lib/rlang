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

sexp* r_expr_protect(sexp* x) {
  static sexp* quote_prim = NULL;
  if (!quote_prim) quote_prim = r_base_ns_get("quote");

  sexp* args = KEEP(r_new_node(x, r_null));
  sexp* out = r_new_call(quote_prim, args);

  FREE(1);
  return out;
}
