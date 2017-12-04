#include "rlang.h"


sexp* r_new_tagged_node(const char* tag, sexp* car, sexp* cdr) {
  sexp* node = KEEP(r_new_node(car, cdr));
  r_node_poke_tag(node, r_sym(tag));
  FREE(1);
  return node;
}

// Shallow copy of a node tree
sexp* r_node_tree_clone(sexp* x) {
  if (r_typeof(x) != r_type_pairlist) {
    r_abort("Internal error: Expected node tree for shallow copy");
  }

  x = KEEP(r_duplicate(x, true));

  sexp* rest = x;
  while (rest != r_null) {
    sexp* head = r_node_car(rest);
    if (r_typeof(head) == r_type_pairlist) {
      r_node_poke_car(rest, r_node_tree_clone(head));
    }
    rest = r_node_cdr(rest);
  }

  FREE(1);
  return x;
}
