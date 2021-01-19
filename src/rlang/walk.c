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


static
bool sexp_iterate_recurse(struct sexp_stack* p_stack,
                          sexp* x,
                          int depth,
                          sexp* parent,
                          enum r_node_relation rel,
                          r_ssize i,
                          sexp_iterator_fn* it,
                          void* data);

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
    dir = R_NODE_DIRECTION_pivot;
    break;
  default:
    dir = R_NODE_DIRECTION_incoming;
    break;
  }

  // Visit the node -- incoming trip
  enum r_sexp_iterate out = it(data, x, type, depth, parent, rel, i, dir);

  switch (out) {
  case R_SEXP_ITERATE_abort: return false;
  case R_SEXP_ITERATE_skip: return true;
  case R_SEXP_ITERATE_next: break;
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
    if (!sexp_iterate_recurse(p_stack, ATTRIB(x), depth, x, R_NODE_RELATION_attrib, 0, it, data)) return false;
  }

  if (dir != R_NODE_DIRECTION_pivot) {
    switch (type) {
    case r_type_closure:
      if (!sexp_iterate_recurse(p_stack, FORMALS(x), depth, x, R_NODE_RELATION_function_fmls, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, BODY(x), depth, x, R_NODE_RELATION_function_body, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, CLOENV(x), depth, x, R_NODE_RELATION_function_env, 0, it, data)) return false;
      break;
    case r_type_environment:
      if (!sexp_iterate_recurse(p_stack, FRAME(x), depth, x, R_NODE_RELATION_environment_frame, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, ENCLOS(x), depth, x, R_NODE_RELATION_environment_enclos, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, HASHTAB(x), depth, x, R_NODE_RELATION_environment_hashtab, 0, it, data)) return false;
      break;
    case r_type_promise:
      if (!sexp_iterate_recurse(p_stack, PRVALUE(x), depth, x, R_NODE_RELATION_promise_value, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, PREXPR(x), depth, x, R_NODE_RELATION_promise_expr, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, PRENV(x), depth, x, R_NODE_RELATION_promise_env, 0, it, data)) return false;
      break;
    case r_type_pointer:
      if (!sexp_iterate_recurse(p_stack, EXTPTR_PROT(x), depth, x, R_NODE_RELATION_pointer_prot, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, EXTPTR_TAG(x), depth, x, R_NODE_RELATION_pointer_tag, 0, it, data)) return false;
      break;

    case r_type_pairlist:
    case r_type_call:
    case r_type_dots: {
      if (!sexp_iterate_recurse(p_stack, TAG(x), depth, x, R_NODE_RELATION_node_tag, 0, it, data)) return false;
      if (!sexp_iterate_recurse(p_stack, CAR(x), depth, x, R_NODE_RELATION_node_car, 0, it, data)) return false;

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
    }

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
        if (!sexp_iterate_recurse(p_stack, p_x[i], depth, x, vec_rel, i, it, data)) return false;
      }
      break;
    }

    default:
      r_abort("Unexpected type `%s` in `sexp_iterate_recurse()`.", r_type_as_c_string(type));
    }
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
  if (dir != R_NODE_DIRECTION_pivot) {
    enum r_sexp_iterate out = it(data, x, type, depth, parent, rel, i, R_NODE_DIRECTION_outgoing);
    if (out == R_SEXP_ITERATE_abort) {
      return false;
    }
  }

  return true;
}


const char* r_node_direction_as_c_string(enum r_node_direction dir) {
  switch (dir) {
  case R_NODE_DIRECTION_pivot: return "pivot";
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
