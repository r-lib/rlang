#include <rlang.h>
#include "walk.h"

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

struct sexp_stack_info {
  sexp* x;
  enum r_type type;

  const enum sexp_iterator_state* p_state;
  sexp* const * v_arr;
  sexp* const * v_arr_end;

  int depth;
  sexp* parent;
  enum r_node_relation rel;
  enum r_node_direction dir;
};

struct sexp_stack {
  sexp* shelter;
  int n;
  int size;
  struct sexp_stack_info v_info[];
};


void sexp_iterate(sexp* x, sexp_iterator_fn* it, void* data) {
  enum r_type type = r_typeof(x);
  enum sexp_iterator_type it_type = sexp_iterator_type(type, x);
  bool has_attrib = sexp_node_attrib(type, x) != r_null;

  if (it_type == SEXP_ITERATOR_TYPE_atomic && !has_attrib) {
    it(data, x, type, 0, r_null, R_NODE_RELATION_root, 0, R_NODE_DIRECTION_leaf);
    return;
  }

  it(data, x, type, 0, r_null, R_NODE_RELATION_root, 0, R_NODE_DIRECTION_incoming);

  struct sexp_stack* p_stack = new_sexp_stack();
  KEEP(p_stack->shelter);

  struct sexp_stack_info root = {
    .x = x,
    .type = type,
    .depth = 0,
    .parent = r_null,
    .rel = R_NODE_RELATION_root
  };
  init_incoming_stack_info(&root, it_type, has_attrib);

  sexp_stack_push(p_stack, root);
  sexp_iterate_recurse(p_stack, it, data);

  FREE(1);
}

static
void sexp_iterate_recurse(struct sexp_stack* p_stack,
                          sexp_iterator_fn* it,
                          void* data) {
 recurse:
  if (!p_stack->n) {
    return;
  }

  struct sexp_stack_info info = sexp_stack_pop(p_stack);

  r_ssize i = -1;
  if (info.v_arr) {
    i = info.v_arr_end - info.v_arr;
  }

  // Visit the node
  enum r_sexp_iterate out = it(data,
                               info.x,
                               info.type,
                               info.depth,
                               info.parent,
                               info.rel,
                               i,
                               info.dir);

  switch (out) {
  case R_SEXP_ITERATE_next:
    break;
  case R_SEXP_ITERATE_skip: {
    if (info.dir == R_NODE_DIRECTION_incoming) {
      --p_stack->n;
    }
    break;
  case R_SEXP_ITERATE_abort:
    return;
  }}

  goto recurse;
}

#define SEXP_STACK_INIT_SIZE 1000

static
struct sexp_stack* new_sexp_stack() {
  r_ssize size = sizeof(struct sexp_stack) + sizeof(struct sexp_stack_info) * SEXP_STACK_INIT_SIZE;
  sexp* shelter = r_new_vector(r_type_raw, size);

  struct sexp_stack* stack = (struct sexp_stack*) r_raw_deref(shelter);

  stack->shelter = shelter;
  stack->n = 0;
  stack->size = SEXP_STACK_INIT_SIZE;

  return stack;
}

static
void sexp_stack_push(struct sexp_stack* p_stack, struct sexp_stack_info info) {
  int i = ++p_stack->n;
  if (i == p_stack->size) {
    r_abort("TODO: Grow stack.");
  }

  p_stack->v_info[i] = info;
}


/*
 * An incoming node has a state indicating which edge we're at. An
 * outgoing node just need to be visited again and then popped. A
 * leaf node is just visited once and then popped.
 */
static inline
struct sexp_stack_info sexp_stack_pop(struct sexp_stack* p_stack) {
  struct sexp_stack_info* p_info = p_stack->v_info + p_stack->n;

  if (p_info->dir != R_NODE_DIRECTION_incoming) {
    --p_stack->n;
    return *p_info;
  }

  enum sexp_iterator_state state = *p_info->p_state;

  struct sexp_stack_info child = { 0 };
  child.parent = p_info->x;
  child.depth = p_info->depth + 1;

  switch (state) {
  case SEXP_ITERATOR_STATE_attrib:
    child.x = r_attrib(p_info->x);
    child.rel = R_NODE_RELATION_attrib;
    break;
  case SEXP_ITERATOR_STATE_elt:
    child.x = *p_info->v_arr;
    child.rel = R_NODE_RELATION_list_elt;
    break;
  case SEXP_ITERATOR_STATE_tag:
    child.x = sexp_node_tag(p_info->type, p_info->x, &child.rel);
    break;
  case SEXP_ITERATOR_STATE_car:
    child.x = sexp_node_car(p_info->type, p_info->x, &child.rel);
    break;
  case SEXP_ITERATOR_STATE_cdr:
    child.x = sexp_node_cdr(p_info->type, p_info->x, &child.rel);
    break;
  case SEXP_ITERATOR_STATE_done:
    r_stop_unreached("sexp_stack_pop");
  }

  child.type = r_typeof(child.x);
  bool has_attrib = sexp_node_attrib(child.type, child.x) != r_null;
  enum sexp_iterator_type it_type = sexp_iterator_type(child.type, child.x);

  if (it_type == SEXP_ITERATOR_TYPE_atomic && !has_attrib) {
    child.p_state = NULL;
    child.dir = R_NODE_DIRECTION_leaf;
  } else {
    init_incoming_stack_info(&child, it_type, has_attrib);

    // Push incoming node on the stack so it can be visited again,
    // either to descend its children or to visit it again on the
    // outgoing trip
    sexp_stack_push(p_stack, child);
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
    p_info->dir = R_NODE_DIRECTION_outgoing;
  }

  return child;
}

static inline
void init_incoming_stack_info(struct sexp_stack_info* p_info,
                              enum sexp_iterator_type it_type,
                              bool has_attrib) {
  p_info->dir = R_NODE_DIRECTION_incoming;

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
    p_info->v_arr = r_vec_deref_const(p_info->x);
    p_info->v_arr_end = p_info->v_arr + r_length(p_info->x);
    p_info->p_state = vector_states + !has_attrib;
    break;
  }
}

static inline
enum sexp_iterator_type sexp_iterator_type(enum r_type type,
                                           sexp* x) {
  switch (type) {
  case r_type_closure:
  case r_type_environment:
  case r_type_promise:
  case r_type_pairlist:
  case r_type_call:
  case r_type_dots:
    return SEXP_ITERATOR_TYPE_node;
  case r_type_pointer:
    return SEXP_ITERATOR_TYPE_pointer;
  case r_type_list:
  case r_type_expression:
  case r_type_character:
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
sexp* sexp_node_attrib(enum r_type type, sexp* x) {
  // Strings have private data stored in attributes
  if (type == r_type_string) {
    return r_null;
  } else {
    return ATTRIB(x);
  }
}
static inline
sexp* sexp_node_car(enum r_type type,
                    sexp* x,
                    enum r_node_relation* p_rel) {
  switch (type) {
  case r_type_closure:     *p_rel = R_NODE_RELATION_function_fmls; return FORMALS(x);
  case r_type_environment: *p_rel = R_NODE_RELATION_environment_frame; return FRAME(x);
  case r_type_promise:     *p_rel = R_NODE_RELATION_promise_value; return PRVALUE(x);
  case r_type_pairlist:
  case r_type_call:
  case r_type_dots:        *p_rel = R_NODE_RELATION_node_car; return CAR(x);
  case r_type_pointer:
  default:                 *p_rel = -1; return r_null;
  }
}
static inline
sexp* sexp_node_cdr(enum r_type type,
                    sexp* x,
                    enum r_node_relation* p_rel) {
  switch (type) {
  case r_type_closure:     *p_rel = R_NODE_RELATION_function_body; return BODY(x);
  case r_type_environment: *p_rel = R_NODE_RELATION_environment_enclos; return ENCLOS(x);
  case r_type_promise:     *p_rel = R_NODE_RELATION_promise_expr; return PREXPR(x);
  case r_type_pointer:     *p_rel = R_NODE_RELATION_pointer_prot; return EXTPTR_PROT(x);
  case r_type_pairlist:
  case r_type_call:
  case r_type_dots:        *p_rel = R_NODE_RELATION_node_cdr; return CDR(x);
  default:                 *p_rel = -1; return r_null;
  }
}
static inline
sexp* sexp_node_tag(enum r_type type,
                    sexp* x,
                    enum r_node_relation* p_rel) {
  switch (type) {
  case r_type_closure:     *p_rel = R_NODE_RELATION_function_env; return CLOENV(x);
  case r_type_environment: *p_rel = R_NODE_RELATION_environment_hashtab; return HASHTAB(x);
  case r_type_promise:     *p_rel = R_NODE_RELATION_promise_env; return PRENV(x);
  case r_type_pointer:     *p_rel = R_NODE_RELATION_pointer_tag; return EXTPTR_TAG(x);
  case r_type_pairlist:
  case r_type_call:
  case r_type_dots:        *p_rel = R_NODE_RELATION_node_tag; return TAG(x);
  default:                 *p_rel = -1; return r_null;
  }
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

  case R_NODE_RELATION_none: r_stop_internal("r_node_relation_as_c_string",
                                             "Found `R_NODE_RELATION_none`.");
  default: r_stop_unreached("r_node_relation_as_c_string");
  }
}

const char* r_node_raw_relation_as_c_string(enum r_node_raw_relation rel) {
  switch (rel) {
  case R_NODE_RAW_RELATION_root: return "root";
  case R_NODE_RAW_RELATION_attrib: return "attrib";
  case R_NODE_RAW_RELATION_node_car: return "node_car";
  case R_NODE_RAW_RELATION_node_cdr: return "node_cdr";
  case R_NODE_RAW_RELATION_node_tag: return "node_tag";
  case R_NODE_RAW_RELATION_vector_elt: return "vector_elt";
  default: r_stop_unreached("r_node_raw_relation_as_c_string");
  }
}
