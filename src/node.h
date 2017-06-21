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

SEXP r_new_node_(SEXP car, SEXP cdr);
SEXP r_new_node(SEXP car, SEXP cdr);
SEXP r_new_pairlist(SEXP car);
SEXP r_new_pairlist2(SEXP car1, SEXP car2);
SEXP r_new_pairlist3(SEXP car1, SEXP car2, SEXP car3);

SEXP r_node_tag(SEXP x);
SEXP r_mut_node_tag(SEXP x, SEXP tag);


#endif
