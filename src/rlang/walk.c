#include <rlang.h>
#include "walk.h"

#define SEXP_STACK_INIT_SIZE 256

enum sexp_iterator_type {
  SEXP_ITERATOR_TYPE_node,
  SEXP_ITERATOR_TYPE_pointer,
  SEXP_ITERATOR_TYPE_vector,
  SEXP_ITERATOR_TYPE_atomic
};

enum sexp_iterator_state {
  SEXP_ITERATOR_STATE_done,
  SEXP_ITERATOR_STATE_attrib,
  SEXP_ITERATOR_STATE_tag,
  SEXP_ITERATOR_STATE_car,
  SEXP_ITERATOR_STATE_cdr,
  SEXP_ITERATOR_STATE_elt
};

struct sexp_stack_info {
  r_obj* x;
  enum r_type type;

  const enum sexp_iterator_state* p_state;
  r_obj* const * v_arr;
  r_obj* const * v_arr_end;

  int depth;
  r_obj* parent;
  enum r_sexp_it_relation rel;
  enum r_sexp_it_direction dir;
};

#include "decl/walk-decl.h"


static
const enum sexp_iterator_state node_states[] = {
  SEXP_ITERATOR_STATE_attrib,
  SEXP_ITERATOR_STATE_tag,
  SEXP_ITERATOR_STATE_car,
  SEXP_ITERATOR_STATE_cdr,
  SEXP_ITERATOR_STATE_done
};
static
const enum sexp_iterator_state pointer_states[] = {
  SEXP_ITERATOR_STATE_attrib,
  SEXP_ITERATOR_STATE_tag,
  SEXP_ITERATOR_STATE_cdr,
  SEXP_ITERATOR_STATE_done
};
static
const enum sexp_iterator_state vector_states[] = {
  SEXP_ITERATOR_STATE_attrib,
  SEXP_ITERATOR_STATE_elt,
  SEXP_ITERATOR_STATE_done
};
static
const enum sexp_iterator_state structure_states[] = {
  SEXP_ITERATOR_STATE_attrib,
  SEXP_ITERATOR_STATE_done
};
static
const enum sexp_iterator_state done_state[] = {
  SEXP_ITERATOR_STATE_done
};


struct r_sexp_iterator* r_new_sexp_iterator(r_obj* root) {
  r_obj* shelter = KEEP(r_alloc_list(2));

  r_obj* it = r_alloc_raw(sizeof(struct r_sexp_iterator));
  r_list_poke(shelter, 0, it);
  struct r_sexp_iterator* p_it = r_raw_begin(it);

  struct r_dyn_array* p_stack = r_new_dyn_array(sizeof(struct sexp_stack_info), SEXP_STACK_INIT_SIZE);
  r_list_poke(shelter, 1, p_stack->shelter);

  enum r_type type = r_typeof(root);
  enum sexp_iterator_type it_type = sexp_iterator_type(type, root);
  bool has_attrib = sexp_node_attrib(type, root) != r_null;

  struct sexp_stack_info root_info = {
    .x = root,
    .type = type,
    .depth = -1,
    .parent = r_null,
    .rel = R_SEXP_IT_RELATION_root
  };

  if (it_type == SEXP_ITERATOR_TYPE_atomic && !has_attrib) {
    root_info.p_state = NULL;
    root_info.dir = R_SEXP_IT_DIRECTION_leaf;
  } else {
    init_incoming_stack_info(&root_info, it_type, has_attrib);
  }

  r_dyn_push_back(p_stack, &root_info);

  *p_it = (struct r_sexp_iterator) {
    .shelter = shelter,
    .p_stack = p_stack,
    .x = r_null,
    .parent = r_null,
  };
  
  FREE(1);
  return p_it;
}

/*
 * An incoming node has a state indicating which edge we're at. An
 * outgoing node just need to be visited again and then popped. A
 * leaf node is just visited once and then popped.
 */
bool r_sexp_next(struct r_sexp_iterator* p_it) {
  struct r_dyn_array* p_stack = p_it->p_stack;
  if (!p_stack->count) {
    return false;
  }

  struct sexp_stack_info* p_info = (struct sexp_stack_info*) r_dyn_last(p_stack);

  if (p_it->skip_incoming) {
    p_it->skip_incoming = false;

    if (p_it->dir == R_SEXP_IT_DIRECTION_incoming) {
      r_dyn_pop_back(p_stack);
      return r_sexp_next(p_it);
    }
  }

  // In the normal case, if we push an "incoming" node on the stack it
  // means that we have already visited it and we are now visiting its
  // children. The root node is signalled with a depth of -1 so it can
  // be visited first before being visited as an incoming node.
  bool root = (p_info->depth == -1);

  if (!root && p_info->dir == R_SEXP_IT_DIRECTION_incoming) {
    return sexp_next_incoming(p_it, p_info);
  }

  r_ssize i = -1;
  if (p_info->v_arr) {
    i = p_info->v_arr_end - p_info->v_arr;
  }
  p_it->x = p_info->x;
  p_it->type = p_info->type;
  p_it->depth = p_info->depth;
  p_it->parent = p_info->parent;
  p_it->rel = p_info->rel;
  p_it->i = i;
  p_it->dir = p_info->dir;

  if (root) {
    ++p_it->depth;
    ++p_info->depth;

    // Incoming visit for the root node
    if (p_it->dir == R_SEXP_IT_DIRECTION_incoming) {
      return true;
    }
  }

  r_dyn_pop_back(p_stack);
  return true;
}

static
bool sexp_next_incoming(struct r_sexp_iterator* p_it,
                        struct sexp_stack_info* p_info) {
  enum sexp_iterator_state state = *p_info->p_state;
  r_obj* x = p_info->x;
  enum r_type type = p_info->type;

  struct sexp_stack_info child = { 0 };
  child.parent = x;
  child.depth = p_info->depth + 1;

  switch (state) {
  case SEXP_ITERATOR_STATE_attrib:
    child.x = r_attrib(x);
    child.rel = R_SEXP_IT_RELATION_attrib;
    break;
  case SEXP_ITERATOR_STATE_elt:
    child.x = *p_info->v_arr;
    child.rel = R_SEXP_IT_RELATION_list_elt;
    break;
  case SEXP_ITERATOR_STATE_tag:
    child.x = sexp_node_tag(type, x, &child.rel);
    break;
  case SEXP_ITERATOR_STATE_car:
    child.x = sexp_node_car(type, x, &child.rel);
    break;
  case SEXP_ITERATOR_STATE_cdr:
    child.x = sexp_node_cdr(type, x, &child.rel);
    break;
  case SEXP_ITERATOR_STATE_done:
    r_stop_unreachable();
  }

  child.type = r_typeof(child.x);
  bool has_attrib = sexp_node_attrib(child.type, child.x) != r_null;
  enum sexp_iterator_type it_type = sexp_iterator_type(child.type, child.x);

  if (it_type == SEXP_ITERATOR_TYPE_atomic && !has_attrib) {
    child.p_state = NULL;
    child.dir = R_SEXP_IT_DIRECTION_leaf;
  } else {
    init_incoming_stack_info(&child, it_type, has_attrib);

    // Push incoming node on the stack so it can be visited again,
    // either to descend its children or to visit it again on the
    // outgoing trip
    r_dyn_push_back(p_it->p_stack, &child);
  }

  // Bump state for next iteration
  if (state == SEXP_ITERATOR_STATE_elt) {
    ++p_info->v_arr;
    if (p_info->v_arr == p_info->v_arr_end) {
      p_info->p_state = done_state;
    }
  } else {
    ++p_info->p_state;
  }

  // Flip incoming to outgoing if we're done visiting children after
  // this iteration. We don't leave a done node on the stack because
  // that would break the invariant that there are remaining nodes to
  // visit when `n > 0` and that the stack can be popped.
  if (*p_info->p_state == SEXP_ITERATOR_STATE_done) {
    p_info->dir = R_SEXP_IT_DIRECTION_outgoing;
  }

  r_ssize i = -1;
  if (child.v_arr) {
    i = child.v_arr_end - child.v_arr;
  }

  p_it->x = child.x;
  p_it->type = child.type;
  p_it->depth = child.depth;
  p_it->parent = child.parent;
  p_it->rel = child.rel;
  p_it->i = i;
  p_it->dir = child.dir;
  return true;
}


static inline
void init_incoming_stack_info(struct sexp_stack_info* p_info,
                              enum sexp_iterator_type it_type,
                              bool has_attrib) {
  p_info->dir = R_SEXP_IT_DIRECTION_incoming;

  switch (it_type) {
  case SEXP_ITERATOR_TYPE_atomic:
    p_info->p_state = structure_states;
    break;
  case SEXP_ITERATOR_TYPE_node:
    p_info->p_state = node_states + !has_attrib;
    break;
  case SEXP_ITERATOR_TYPE_pointer:
    p_info->p_state = pointer_states + !has_attrib;
    break;
  case SEXP_ITERATOR_TYPE_vector:
    p_info->v_arr = r_vec_cbegin(p_info->x);
    p_info->v_arr_end = p_info->v_arr + r_length(p_info->x);
    p_info->p_state = vector_states + !has_attrib;
    break;
  }
}

static inline
enum sexp_iterator_type sexp_iterator_type(enum r_type type,
                                           r_obj* x) {
  switch (type) {
  case R_TYPE_closure:
  case R_TYPE_environment:
  case R_TYPE_promise:
  case R_TYPE_pairlist:
  case R_TYPE_call:
  case R_TYPE_dots:
    return SEXP_ITERATOR_TYPE_node;
  case R_TYPE_pointer:
    return SEXP_ITERATOR_TYPE_pointer;
  case R_TYPE_list:
  case R_TYPE_expression:
  case R_TYPE_character:
    if (r_length(x)) {
      return SEXP_ITERATOR_TYPE_vector;
    } else {
      return SEXP_ITERATOR_TYPE_atomic;
    }
  default:
    return SEXP_ITERATOR_TYPE_atomic;
  }
}
static inline
r_obj* sexp_node_attrib(enum r_type type, r_obj* x) {
  // Strings have private data stored in attributes
  if (type == R_TYPE_string) {
    return r_null;
  } else {
    return ATTRIB(x);
  }
}
static inline
r_obj* sexp_node_car(enum r_type type,
                     r_obj* x,
                     enum r_sexp_it_relation* p_rel) {
  switch (type) {
  case R_TYPE_closure:     *p_rel = R_SEXP_IT_RELATION_function_fmls; return FORMALS(x);
  case R_TYPE_environment: *p_rel = R_SEXP_IT_RELATION_environment_frame; return FRAME(x);
  case R_TYPE_promise:     *p_rel = R_SEXP_IT_RELATION_promise_value; return PRVALUE(x);
  case R_TYPE_pairlist:
  case R_TYPE_call:
  case R_TYPE_dots:        *p_rel = R_SEXP_IT_RELATION_node_car; return CAR(x);
  case R_TYPE_pointer:
  default:                 *p_rel = -1; return r_null;
  }
}
static inline
r_obj* sexp_node_cdr(enum r_type type,
                     r_obj* x,
                     enum r_sexp_it_relation* p_rel) {
  switch (type) {
  case R_TYPE_closure:     *p_rel = R_SEXP_IT_RELATION_function_body; return BODY(x);
  case R_TYPE_environment: *p_rel = R_SEXP_IT_RELATION_environment_enclos; return ENCLOS(x);
  case R_TYPE_promise:     *p_rel = R_SEXP_IT_RELATION_promise_expr; return PREXPR(x);
  case R_TYPE_pointer:     *p_rel = R_SEXP_IT_RELATION_pointer_prot; return EXTPTR_PROT(x);
  case R_TYPE_pairlist:
  case R_TYPE_call:
  case R_TYPE_dots:        *p_rel = R_SEXP_IT_RELATION_node_cdr; return CDR(x);
  default:                 *p_rel = -1; return r_null;
  }
}
static inline
r_obj* sexp_node_tag(enum r_type type,
                     r_obj* x,
                     enum r_sexp_it_relation* p_rel) {
  switch (type) {
  case R_TYPE_closure:     *p_rel = R_SEXP_IT_RELATION_function_env; return CLOENV(x);
  case R_TYPE_environment: *p_rel = R_SEXP_IT_RELATION_environment_hashtab; return HASHTAB(x);
  case R_TYPE_promise:     *p_rel = R_SEXP_IT_RELATION_promise_env; return PRENV(x);
  case R_TYPE_pointer:     *p_rel = R_SEXP_IT_RELATION_pointer_tag; return EXTPTR_TAG(x);
  case R_TYPE_pairlist:
  case R_TYPE_call:
  case R_TYPE_dots:        *p_rel = R_SEXP_IT_RELATION_node_tag; return TAG(x);
  default:                 *p_rel = -1; return r_null;
  }
}


const char* r_sexp_it_direction_as_c_string(enum r_sexp_it_direction dir) {
  switch (dir) {
  case R_SEXP_IT_DIRECTION_leaf: return "leaf";
  case R_SEXP_IT_DIRECTION_incoming: return "incoming";
  case R_SEXP_IT_DIRECTION_outgoing: return "outgoing";
  default: r_stop_unreachable();
  }
}

const char* r_sexp_it_relation_as_c_string(enum r_sexp_it_relation rel) {
  switch (rel) {
  case R_SEXP_IT_RELATION_root: return "root";
  case R_SEXP_IT_RELATION_attrib: return "attrib";

  case R_SEXP_IT_RELATION_node_car: return "node_car";
  case R_SEXP_IT_RELATION_node_cdr: return "node_cdr";
  case R_SEXP_IT_RELATION_node_tag: return "node_tag";

  case R_SEXP_IT_RELATION_symbol_string: return "symbol_string";
  case R_SEXP_IT_RELATION_symbol_value: return "symbol_value";
  case R_SEXP_IT_RELATION_symbol_internal: return "symbol_internal";

  case R_SEXP_IT_RELATION_function_fmls: return "function_fmls";
  case R_SEXP_IT_RELATION_function_body: return "function_body";
  case R_SEXP_IT_RELATION_function_env: return "function_env";

  case R_SEXP_IT_RELATION_environment_frame: return "environment_frame";
  case R_SEXP_IT_RELATION_environment_enclos: return "environment_enclos";
  case R_SEXP_IT_RELATION_environment_hashtab: return "environment_hashtab";

  case R_SEXP_IT_RELATION_promise_value: return "promise_value";
  case R_SEXP_IT_RELATION_promise_expr: return "promise_expr";
  case R_SEXP_IT_RELATION_promise_env: return "promise_env";

  case R_SEXP_IT_RELATION_pointer_prot: return "pointer_prot";
  case R_SEXP_IT_RELATION_pointer_tag: return "pointer_tag";

  case R_SEXP_IT_RELATION_list_elt: return "list_elt";
  case R_SEXP_IT_RELATION_character_elt: return "character_elt";
  case R_SEXP_IT_RELATION_expression_elt: return "expression_elt";

  case R_SEXP_IT_RELATION_none: r_stop_internal("r_sexp_it_relation_as_c_string",
                                                "Found `R_SEXP_IT_RELATION_none`.");
  default: r_stop_unreachable();
  }
}

const char* r_sexp_it_raw_relation_as_c_string(enum r_sexp_it_raw_relation rel) {
  switch (rel) {
  case R_SEXP_IT_RAW_RELATION_root: return "root";
  case R_SEXP_IT_RAW_RELATION_attrib: return "attrib";
  case R_SEXP_IT_RAW_RELATION_node_car: return "node_car";
  case R_SEXP_IT_RAW_RELATION_node_cdr: return "node_cdr";
  case R_SEXP_IT_RAW_RELATION_node_tag: return "node_tag";
  case R_SEXP_IT_RAW_RELATION_vector_elt: return "vector_elt";
  default: r_stop_unreachable();
  }
}
