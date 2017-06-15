#ifndef RLANG_NODE_H
#define RLANG_NODE_H


SEXP r_node_car(SEXP x);
SEXP r_node_cdr(SEXP x);
SEXP r_node_caar(SEXP x);
SEXP r_node_cadr(SEXP x);
SEXP r_node_cdar(SEXP x);
SEXP r_node_cddr(SEXP x);

SEXP r_mut_node_car(SEXP x, SEXP newcar);
SEXP r_mut_node_cdr(SEXP x, SEXP newcdr);
SEXP r_mut_node_caar(SEXP x, SEXP newcaar);
SEXP r_mut_node_cadr(SEXP x, SEXP newcar);
SEXP r_mut_node_cdar(SEXP x, SEXP newcdar);
SEXP r_mut_node_cddr(SEXP x, SEXP newcdr);

SEXP r_cons(SEXP car, SEXP cdr);

SEXP r_duplicate(SEXP x);
SEXP r_shallow_duplicate(SEXP x);

SEXP r_node_tag(SEXP x);
SEXP r_mut_node_tag(SEXP x, SEXP tag);


#endif
