#include "rlang.h"


// Shallow copy of a node tree
sexp* r_node_tree_clone(sexp* x) {
  if (r_typeof(x) != R_TYPE_pairlist) {
    r_abort("Internal error: Expected node tree for shallow copy");
  }

  x = KEEP(r_clone(x));

  sexp* rest = x;
  while (rest != r_null) {
    sexp* head = r_node_car(rest);
    if (r_typeof(head) == R_TYPE_pairlist) {
      r_node_poke_car(rest, r_node_tree_clone(head));
    }
    rest = r_node_cdr(rest);
  }

  FREE(1);
  return x;
}

sexp* r_pairlist_find(sexp* node, sexp* tag) {
  while (node != r_null) {
    if (r_node_tag(node) == tag) {
      return node;
    }
    node = r_node_cdr(node);
  }

  return r_null;
}

sexp* r_pairlist_rev(sexp* node) {
  if (node == r_null) {
    return node;
  }

  sexp* prev = r_null;
  sexp* tail = node;
  sexp* next;
  while (tail != r_null) {
    next = r_node_cdr(tail);
    r_node_poke_cdr(tail, prev);
    prev = tail;
    tail = next;
  }

  return prev;
}
