#include "rlang.h"


SEXP r_new_tagged_node(const char* tag, SEXP car, SEXP cdr) {
  SEXP node = KEEP(r_new_node(car, cdr));
  r_node_poke_tag(node, r_sym(tag));
  FREE(1);
  return node;
}
