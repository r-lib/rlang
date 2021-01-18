#include <rlang.h>
#include "walk.h"

static
bool sexp_iterate_recurse(sexp* x,
                          int depth,
                          sexp* parent,
                          enum r_node_relation rel,
                          r_ssize i,
                          sexp_iterator_fn* it,
                          void* data);


void sexp_iterate(sexp* x, sexp_iterator_fn* it, void* data) {
  sexp_iterate_recurse(x, 0, r_null, R_NODE_RELATION_root, 0, it, data);
}

static
bool sexp_iterate_recurse(sexp* x,
                          int depth,
                          sexp* parent,
                          enum r_node_relation rel,
                          r_ssize i,
                          sexp_iterator_fn* it,
                          void* data) {
 recurse:
  if (depth % 10 == 0) {
    R_CheckStack();
  }

  enum r_type type = r_typeof(x);
  enum r_node_direction dir;

  switch (type) {
  case r_type_null:
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_raw:
  case r_type_special:
  case r_type_builtin:
  case r_type_string:
  case r_type_symbol:
  case r_type_s4:
  case r_type_bytecode:
  case r_type_weakref:
    dir = R_NODE_DIRECTION_leaf;
    break;
  default:
    dir = R_NODE_DIRECTION_incoming;
    break;
  }

  if (!it(data, x, type, depth, parent, rel, i, dir)) return false;

  if (dir == R_NODE_DIRECTION_leaf) {
    return true;
  }

  ++depth;

  // Recursing on the attributes of `NULL`causes an infinite
  // recursion. The attributes of strings contain private data for the
  // garbage collector.
  switch (r_typeof(x)) {
  case r_type_null:
  case r_type_string:
    break;
  default:
    if (!sexp_iterate_recurse(ATTRIB(x), depth, x, R_NODE_RELATION_attrib, 0, it, data)) return false;
  }

  switch (type) {
  case r_type_closure:
    if (!sexp_iterate_recurse(FORMALS(x), depth, x, R_NODE_RELATION_function_fmls, 0, it, data)) return false;
    if (!sexp_iterate_recurse(BODY(x), depth, x, R_NODE_RELATION_function_body, 0, it, data)) return false;
    if (!sexp_iterate_recurse(CLOENV(x), depth, x, R_NODE_RELATION_function_env, 0, it, data)) return false;
    break;
  case r_type_environment:
    if (!sexp_iterate_recurse(FRAME(x), depth, x, R_NODE_RELATION_environment_frame, 0, it, data)) return false;
    if (!sexp_iterate_recurse(ENCLOS(x), depth, x, R_NODE_RELATION_environment_enclos, 0, it, data)) return false;
    if (!sexp_iterate_recurse(HASHTAB(x), depth, x, R_NODE_RELATION_environment_hashtab, 0, it, data)) return false;
    break;
  case r_type_promise:
    if (!sexp_iterate_recurse(PRVALUE(x), depth, x, R_NODE_RELATION_promise_value, 0, it, data)) return false;
    if (!sexp_iterate_recurse(PREXPR(x), depth, x, R_NODE_RELATION_promise_expr, 0, it, data)) return false;
    if (!sexp_iterate_recurse(PRENV(x), depth, x, R_NODE_RELATION_promise_env, 0, it, data)) return false;
    break;
  case r_type_pointer:
    if (!sexp_iterate_recurse(EXTPTR_PROT(x), depth, x, R_NODE_RELATION_pointer_prot, 0, it, data)) return false;
    if (!sexp_iterate_recurse(EXTPTR_TAG(x), depth, x, R_NODE_RELATION_pointer_tag, 0, it, data)) return false;
    break;

  case r_type_pairlist:
  case r_type_call:
  case r_type_dots:
    if (!sexp_iterate_recurse(TAG(x), depth, x, R_NODE_RELATION_node_tag, 0, it, data)) return false;
    if (!sexp_iterate_recurse(CAR(x), depth, x, R_NODE_RELATION_node_car, 0, it, data)) return false;
    parent = x;
    x = CDR(x);
    rel = R_NODE_RELATION_node_cdr;
    goto recurse;
    // FIXME: Problematic for outgoing direction

  case r_type_list:
  case r_type_expression:
  case r_type_character: {
    r_ssize n = r_length(x);

    sexp* const * p_x;
    if (type == r_type_character) {
      p_x = r_chr_deref_const(x);
    } else {
      p_x = r_list_deref_const(x);
    }

    enum r_node_relation vec_rel = 0;
    switch (type) {
    case r_type_list: vec_rel = R_NODE_RELATION_list_elt; break;
    case r_type_expression: vec_rel = R_NODE_RELATION_expression_elt; break;
    case r_type_character: vec_rel = R_NODE_RELATION_character_elt; break;
    default: r_stop_internal("sexp_iterate_recurse", "while setting `vec_rel`.");
    }

    for (r_ssize i = 0; i < n; ++i) {
      if (!sexp_iterate_recurse(p_x[i], depth, x, vec_rel, i, it, data)) return false;
    }
    break;
  }

  default:
    r_abort("Unexpected type `%s` in `sexp_iterate_recurse()`.", r_type_as_c_string(type));
  }

  // Visit node a second time on the way back
  if (!it(data, x, type, depth, parent, rel, i, R_NODE_DIRECTION_outgoing)) return false;

  return true;
}


const char* r_node_direction_as_c_string(enum r_node_direction dir) {
  switch (dir) {
  case R_NODE_DIRECTION_leaf: return "leaf";
  case R_NODE_DIRECTION_incoming: return "incoming";
  case R_NODE_DIRECTION_outgoing: return "outgoing";
  default: r_stop_unreached("r_node_direction_as_c_string");
  }
}

const char* r_node_relation_as_c_string(enum r_node_relation rel) {
  switch (rel) {
  case R_NODE_RELATION_root: return "root";
  case R_NODE_RELATION_attrib: return "attrib";

  case R_NODE_RELATION_node_car: return "node_car";
  case R_NODE_RELATION_node_cdr: return "node_cdr";
  case R_NODE_RELATION_node_tag: return "node_tag";

  case R_NODE_RELATION_symbol_string: return "symbol_string";
  case R_NODE_RELATION_symbol_value: return "symbol_value";
  case R_NODE_RELATION_symbol_internal: return "symbol_internal";

  case R_NODE_RELATION_function_fmls: return "function_fmls";
  case R_NODE_RELATION_function_body: return "function_body";
  case R_NODE_RELATION_function_env: return "function_env";

  case R_NODE_RELATION_environment_frame: return "environment_frame";
  case R_NODE_RELATION_environment_enclos: return "environment_enclos";
  case R_NODE_RELATION_environment_hashtab: return "environment_hashtab";

  case R_NODE_RELATION_promise_value: return "promise_value";
  case R_NODE_RELATION_promise_expr: return "promise_expr";
  case R_NODE_RELATION_promise_env: return "promise_env";

  case R_NODE_RELATION_pointer_prot: return "pointer_prot";
  case R_NODE_RELATION_pointer_tag: return "pointer_tag";

  case R_NODE_RELATION_list_elt: return "list_elt";
  case R_NODE_RELATION_character_elt: return "character_elt";
  case R_NODE_RELATION_expression_elt: return "expression_elt";

  default: r_stop_unreached("r_node_relation_as_c_string");
  }
}
