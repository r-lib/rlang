#ifndef RLANG_INTERNAL_WALK_H
#define RLANG_INTERNAL_WALK_H

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
enum r_sexp_it_direction {
  R_SEXP_IT_DIRECTION_leaf = 0,
  R_SEXP_IT_DIRECTION_incoming,
  R_SEXP_IT_DIRECTION_outgoing
};

enum r_sexp_it_relation {
  R_SEXP_IT_RELATION_none = -1,
  R_SEXP_IT_RELATION_root = 0,

  R_SEXP_IT_RELATION_attrib,

  // Nodes
  R_SEXP_IT_RELATION_node_car,
  R_SEXP_IT_RELATION_node_cdr,
  R_SEXP_IT_RELATION_node_tag,

  R_SEXP_IT_RELATION_symbol_string,
  R_SEXP_IT_RELATION_symbol_value,
  R_SEXP_IT_RELATION_symbol_internal,

  R_SEXP_IT_RELATION_function_fmls,
  R_SEXP_IT_RELATION_function_body,
  R_SEXP_IT_RELATION_function_env,

  R_SEXP_IT_RELATION_environment_frame,
  R_SEXP_IT_RELATION_environment_enclos,
  R_SEXP_IT_RELATION_environment_hashtab,

  R_SEXP_IT_RELATION_promise_value,
  R_SEXP_IT_RELATION_promise_expr,
  R_SEXP_IT_RELATION_promise_env,

  R_SEXP_IT_RELATION_pointer_prot,
  R_SEXP_IT_RELATION_pointer_tag,

  // Vectors
  R_SEXP_IT_RELATION_list_elt,
  R_SEXP_IT_RELATION_character_elt,
  R_SEXP_IT_RELATION_expression_elt
};

enum r_sexp_it_raw_relation {
  R_SEXP_IT_RAW_RELATION_root = 0,
  R_SEXP_IT_RAW_RELATION_attrib,
  R_SEXP_IT_RAW_RELATION_node_tag,
  R_SEXP_IT_RAW_RELATION_node_car,
  R_SEXP_IT_RAW_RELATION_node_cdr,
  R_SEXP_IT_RAW_RELATION_vector_elt
};


struct r_sexp_iterator {
  r_obj* shelter;
  bool skip_incoming;

  r_obj* x;
  enum r_type type;
  int depth;
  r_obj* parent;
  enum r_sexp_it_relation rel;
  r_ssize i;
  enum r_sexp_it_direction dir;

  /* private: */
  struct r_dyn_array* p_stack;
};

struct r_sexp_iterator* r_new_sexp_iterator(r_obj* root);

bool r_sexp_next(struct r_sexp_iterator* p_it);
bool r_sexp_skip(struct r_sexp_iterator* p_it);


static inline
enum r_sexp_it_raw_relation r_sexp_it_raw_relation(enum r_sexp_it_relation rel) {
  switch (rel) {
  case R_SEXP_IT_RELATION_root:
    return R_SEXP_IT_RAW_RELATION_root;

  case R_SEXP_IT_RELATION_attrib:
    return R_SEXP_IT_RAW_RELATION_attrib;

  case R_SEXP_IT_RELATION_node_car:
  case R_SEXP_IT_RELATION_symbol_string:
  case R_SEXP_IT_RELATION_environment_frame:
  case R_SEXP_IT_RELATION_function_fmls:
  case R_SEXP_IT_RELATION_promise_value:
    return R_SEXP_IT_RAW_RELATION_node_car;

  case R_SEXP_IT_RELATION_node_cdr:
  case R_SEXP_IT_RELATION_symbol_value:
  case R_SEXP_IT_RELATION_environment_enclos:
  case R_SEXP_IT_RELATION_function_body:
  case R_SEXP_IT_RELATION_promise_expr:
  case R_SEXP_IT_RELATION_pointer_prot:
    return R_SEXP_IT_RAW_RELATION_node_cdr;

  case R_SEXP_IT_RELATION_node_tag:
  case R_SEXP_IT_RELATION_symbol_internal:
  case R_SEXP_IT_RELATION_environment_hashtab:
  case R_SEXP_IT_RELATION_function_env:
  case R_SEXP_IT_RELATION_promise_env:
  case R_SEXP_IT_RELATION_pointer_tag:
    return R_SEXP_IT_RAW_RELATION_node_tag;

  case R_SEXP_IT_RELATION_list_elt:
  case R_SEXP_IT_RELATION_character_elt:
  case R_SEXP_IT_RELATION_expression_elt:
    return R_SEXP_IT_RAW_RELATION_vector_elt;

  default:
    r_abort("Unimplemented type.");
  }
}

const char* r_sexp_it_direction_as_c_string(enum r_sexp_it_direction dir);
const char* r_sexp_it_relation_as_c_string(enum r_sexp_it_relation rel);
const char* r_sexp_it_raw_relation_as_c_string(enum r_sexp_it_raw_relation rel);


#endif
