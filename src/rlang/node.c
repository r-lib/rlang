#include "rlang.h"


sexp* r_new_tagged_node(const char* tag, sexp* car, sexp* cdr) {
  sexp* node = KEEP(r_new_node(car, cdr));
  r_node_poke_tag(node, r_sym(tag));
  FREE(1);
  return node;
}
