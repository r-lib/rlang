#include "rlang.h"


// Shallow copy of a node tree
sexp* r_node_tree_clone(sexp* x) {
  if (r_typeof(x) != r_type_pairlist) {
    r_abort("Internal error: Expected node tree for shallow copy");
  }

  x = KEEP(r_clone(x));

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

/**
 * - If `sentinel` is found in the first node: `parent_out` is `r_null`
 * - If `sentinel` is not found: both return value and `parent_out`
 *   are `r_null`
 * - If `sentinel` is `r_null`, this is like a full shallow duplication
 *   but returns tail node
 */
sexp* r_pairlist_clone_until(sexp* node, sexp* sentinel, sexp** parent_out) {
  sexp* parent = r_null;
  sexp* cur = node;
  int n_protect = 0;

  while (true) {
    if (cur == sentinel) {
      FREE(n_protect);
      *parent_out = parent;
      return node;
    }
    // Return NULL if sentinel is not found
    if (cur == r_null) {
      FREE(n_protect);
      *parent_out = r_null;
      return r_null;
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

  r_abort("Internal error in r_pairlist_clone_until()");
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
