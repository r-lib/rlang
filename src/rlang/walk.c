#include <rlang.h>
#include "walk.h"
#include "decl/walk-decl.h"

struct sexp_stack_info {
  sexp* x;
  int depth;
  sexp* parent;
  enum r_node_relation rel;
};

struct sexp_stack {
  sexp* shelter;
  int n;
  int size;
  struct sexp_stack_info v_info[];
};


void sexp_iterate(sexp* x, sexp_iterator_fn* it, void* data) {
  struct sexp_stack* p_stack = new_sexp_stack();
  KEEP(p_stack->shelter);

  sexp_iterate_recurse(p_stack, x, 0, r_null, R_NODE_RELATION_root, 0, it, data);

  FREE(1);
}

static
bool sexp_iterate_recurse(struct sexp_stack* p_stack,
                          sexp* x,
                          int depth,
                          sexp* parent,
                          enum r_node_relation rel,
                          r_ssize i,
                          sexp_iterator_fn* it,
                          void* data) {
  // This is the number of pairlist nodes on the heap stack. We
  // maintain this stack on the heap to allow tail-call recursion with
  // a goto. The stack is used to visit the pairlist nodes on the
  // outgoing trip.
  int heap_stack_n = 0;

 recurse:
  if (depth % 10 == 0) {
    R_CheckStack();
  }

  enum r_type type = r_typeof(x);
  enum r_node_direction dir;

  enum r_node_relation tag_rel = 0;
  enum r_node_relation car_rel = 0;
  enum r_node_relation cdr_rel = 0;
  enum r_node_relation arr_rel = 0;

  sexp* attrib = sexp_node_attrib(x, type);
  sexp* tag = sexp_node_tag(x, type, &tag_rel);
  sexp* car = sexp_node_car(x, type, &car_rel);
  sexp* cdr = sexp_node_cdr(x, type, &cdr_rel);
  sexp* const * v_arr = sexp_node_arr(x, type, &arr_rel);

  if (attrib != r_null || tag != r_null || car != r_null || cdr != r_null || v_arr != NULL) {
    dir = R_NODE_DIRECTION_incoming;
  } else {
    dir = R_NODE_DIRECTION_leaf;
  }

  // Visit the node -- incoming trip
  enum r_sexp_iterate out = it(data, x, type, depth, parent, rel, i, dir);

  switch (out) {
  case R_SEXP_ITERATE_abort: return false;
  case R_SEXP_ITERATE_skip: return true;
  case R_SEXP_ITERATE_next: break;
  }

  ++depth;

  if (attrib != r_null) {
    if (!sexp_iterate_recurse(p_stack, attrib, depth, x, R_NODE_RELATION_attrib, 0, it, data)) return false;
  }

  if (v_arr != NULL) {
      r_ssize n = r_length(x);
      for (r_ssize i = 0; i < n; ++i) {
        if (!sexp_iterate_recurse(p_stack, v_arr[i], depth, x, arr_rel, i, it, data)) return false;
      }
  }

  if (tag != r_null) {
    if (!sexp_iterate_recurse(p_stack, tag, depth, x, tag_rel, 0, it, data)) return false;
  }
  if (car != r_null) {
    if (!sexp_iterate_recurse(p_stack, car, depth, x, car_rel, 0, it, data)) return false;
  }

  if (cdr != r_null) {
    switch (type) {
    default:
      if (!sexp_iterate_recurse(p_stack, cdr, depth, x, cdr_rel, 0, it, data)) return false;
      break;
    case r_type_pairlist:
    case r_type_call:
    case r_type_dots: {
      // We're going to make a tail call goto to recurse into the
      // pairlist CDR. First save the current node value so we can visit
      // it back on the outgoing trip.
      struct sexp_stack_info x_info = {
        .x = x,
        .depth = depth,
        .parent = parent,
        .rel = rel
      };
      sexp_stack_push(p_stack, x_info);
      ++heap_stack_n;

      // Now set the CDR as current value and recurse
      parent = x;
      x = CDR(x);
      rel = R_NODE_RELATION_node_cdr;
      ++depth;
      goto recurse;
    }}
  }

  // Visit node a second time on the way back (outgoing direction).
  // Start with the pairlist nodes pushed on the heap stack.
  for (int i = 0; i < heap_stack_n; ++i) {
    struct sexp_stack_info info = sexp_stack_pop(p_stack);
    if (!it(data, info.x, r_typeof(info.x), info.depth, info.parent, R_NODE_RELATION_node_cdr, 0, R_NODE_DIRECTION_outgoing)) {
      // Pop the remaining stack
      // TODO: Test this
      p_stack->n -= heap_stack_n - (i + 1);
      return false;
    }
  }

  // Visit the node -- outgoing trip
  if (dir != R_NODE_DIRECTION_leaf) {
    enum r_sexp_iterate out = it(data, x, type, depth, parent, rel, i, R_NODE_DIRECTION_outgoing);
    if (out == R_SEXP_ITERATE_abort) {
      return false;
    }
  }

  return true;
}

static inline
sexp* sexp_node_attrib(sexp* x, enum r_type type) {
  // Strings have private data stored in attributes
  switch (type) {
  case r_type_string:      return r_null;
  default:                 return ATTRIB(x);
  }
}
static inline
sexp* sexp_node_car(sexp* x, enum r_type type,
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
sexp* sexp_node_cdr(sexp* x, enum r_type type,
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
sexp* sexp_node_tag(sexp* x, enum r_type type,
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
static inline
sexp* const * sexp_node_arr(sexp* x, enum r_type type,
                            enum r_node_relation* p_rel) {
  switch (type) {
  case r_type_list:
    *p_rel = R_NODE_RELATION_list_elt;
    return r_list_deref_const(x);
  case r_type_expression:
    *p_rel = R_NODE_RELATION_expression_elt;
    return r_list_deref_const(x);
  case r_type_character:
    *p_rel = R_NODE_RELATION_character_elt;
    return r_chr_deref_const(x);
  default:
    return NULL;
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
  default: r_stop_unreached("r_node_relation_as_c_string");
  }
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

static inline
struct sexp_stack_info sexp_stack_pop(struct sexp_stack* p_stack) {
  return p_stack->v_info[p_stack->n--];
}
