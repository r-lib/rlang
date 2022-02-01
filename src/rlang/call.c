#include "rlang.h"

static r_obj* quote_prim = NULL;


bool r_is_call(r_obj* x, const char* name) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  } else {
    return name == NULL || r_is_symbol(r_node_car(x), name);
  }
}

bool r_is_call_any(r_obj* x, const char** names, int n) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  } else {
    return r_is_symbol_any(r_node_car(x), names, n);
  }
}

r_obj* r_expr_protect(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_symbol:
  case R_TYPE_call:
  case R_TYPE_promise:
    return r_call2(quote_prim, x);
  default:
    return x;
  }
}

static inline
bool is_node(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_call:
  case R_TYPE_pairlist:
    return true;
  default:
    return false;
  }
}

r_obj* r_call_clone(r_obj* x) {
  if (!is_node(x)) {
    r_abort("Input must be a call.");
  }

  x = KEEP(r_clone(x));

  r_obj* rest = x;
  while (rest != r_null) {
    r_obj* head = r_node_car(rest);
    if (is_node(head)) {
      r_node_poke_car(rest, r_call_clone(head));
    }
    rest = r_node_cdr(rest);
  }

  FREE(1);
  return x;
}


void r_init_library_call() {
  quote_prim = r_base_ns_get("quote");
}
