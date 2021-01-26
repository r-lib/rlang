#ifndef RLANG_INTERNAL_WALK_H
#define RLANG_INTERNAL_WALK_H

/**
 * Return value of an iterator callback
 *
 * - `abort`: Interrupt iteration. This causes a full unwind back to
 *   the caller of `r_sexp_iterate()`.
 * - `next`: Continue iteration.
 * - `skip`: Skip the children of an incoming node. This has no effect
 *   when visiting a leaf or a node on an outgoing trip.
 */
enum r_sexp_iterate {
  R_SEXP_ITERATE_abort = 0,
  R_SEXP_ITERATE_next,
  R_SEXP_ITERATE_skip
};

/**
 * Direction of iteration
 *
 * Each non-leaf node of the sexp tree is visited twice: First before
 * visiting the children, and again after the children have been
 * visited. See
 * <https://stlab.adobe.com/group__asl__tutorials__forest.html> about
 * this iteration process.
 *
 * There are three directions:
 * - Incoming: The first time a non-leaf node is visited.
 * - Leaf: After reaching a leaf node, the direction changes from
 *   incoming to outgoing.
 * - Outgoing: The second time a non-leaf node is visited on the way back.
 */
enum r_node_direction {
  R_NODE_DIRECTION_leaf = 0,
  R_NODE_DIRECTION_incoming,
  R_NODE_DIRECTION_outgoing
};

enum r_node_relation {
  R_NODE_RELATION_root = 0,
  R_NODE_RELATION_attrib,

  // Nodes
  R_NODE_RELATION_node_car,
  R_NODE_RELATION_node_cdr,
  R_NODE_RELATION_node_tag,

  R_NODE_RELATION_symbol_string,
  R_NODE_RELATION_symbol_value,
  R_NODE_RELATION_symbol_internal,

  R_NODE_RELATION_function_fmls,
  R_NODE_RELATION_function_body,
  R_NODE_RELATION_function_env,

  R_NODE_RELATION_environment_frame,
  R_NODE_RELATION_environment_enclos,
  R_NODE_RELATION_environment_hashtab,

  R_NODE_RELATION_promise_value,
  R_NODE_RELATION_promise_expr,
  R_NODE_RELATION_promise_env,

  R_NODE_RELATION_pointer_prot,
  R_NODE_RELATION_pointer_tag,

  // Vectors
  R_NODE_RELATION_list_elt,
  R_NODE_RELATION_character_elt,
  R_NODE_RELATION_expression_elt
};

enum r_node_raw_relation {
  R_NODE_RAW_RELATION_root = 0,
  R_NODE_RAW_RELATION_attrib,
  R_NODE_RAW_RELATION_node_tag,
  R_NODE_RAW_RELATION_node_car,
  R_NODE_RAW_RELATION_node_cdr,
  R_NODE_RAW_RELATION_vector_elt,
};


typedef
enum r_sexp_iterate (sexp_iterator_fn)(void* data,
                                       sexp* x,
                                       enum r_type type,
                                       int depth,
                                       sexp* parent,
                                       enum r_node_relation rel,
                                       r_ssize i,
                                       enum r_node_direction dir);

void sexp_iterate(sexp* x, sexp_iterator_fn* it, void* data);


static inline
enum r_node_raw_relation r_node_raw_relation(enum r_node_relation rel) {
  switch (rel) {
  case R_NODE_RELATION_root:
    return R_NODE_RAW_RELATION_root;

  case R_NODE_RELATION_attrib:
    return R_NODE_RAW_RELATION_attrib;

  case R_NODE_RELATION_node_car:
  case R_NODE_RELATION_symbol_string:
  case R_NODE_RELATION_environment_frame:
  case R_NODE_RELATION_function_fmls:
  case R_NODE_RELATION_promise_value:
    return R_NODE_RAW_RELATION_node_car;

  case R_NODE_RELATION_node_cdr:
  case R_NODE_RELATION_symbol_value:
  case R_NODE_RELATION_environment_enclos:
  case R_NODE_RELATION_function_body:
  case R_NODE_RELATION_promise_expr:
  case R_NODE_RELATION_pointer_prot:
    return R_NODE_RAW_RELATION_node_cdr;

  case R_NODE_RELATION_node_tag:
  case R_NODE_RELATION_symbol_internal:
  case R_NODE_RELATION_environment_hashtab:
  case R_NODE_RELATION_function_env:
  case R_NODE_RELATION_promise_env:
  case R_NODE_RELATION_pointer_tag:
    return R_NODE_RAW_RELATION_node_tag;

  case R_NODE_RELATION_list_elt:
  case R_NODE_RELATION_character_elt:
  case R_NODE_RELATION_expression_elt:
    return R_NODE_RAW_RELATION_vector_elt;

  default:
    r_abort("Unimplemented type.");
  }
}

const char* r_node_direction_as_c_string(enum r_node_direction dir);
const char* r_node_relation_as_c_string(enum r_node_relation rel);


#endif
