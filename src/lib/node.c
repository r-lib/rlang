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

sexp* r_node_list_clone_until(sexp* node, sexp* sentinel, sexp** parent_out) {
  sexp* parent = r_null;
  sexp* cur = node;
  int n_protect = 0;

  while (true) {
    if (cur == r_null) {
      // Return NULL parent if sentinel is not found
      parent = r_null;
      break;
    }
    if (cur == sentinel) {
      break;
    }

    sexp* tag = r_node_tag(cur);
    cur = r_new_node(r_node_car(cur), r_node_cdr(cur));
    r_node_poke_tag(cur, tag);

    if (parent == r_null) {
      KEEP_N(cur, n_protect);
      node = cur;
    } else {
      r_node_poke_cdr(parent, cur);
    }

    parent = cur;
    cur = r_node_cdr(cur);
  }

  FREE(n_protect);
  *parent_out = parent;
  return node;
}
