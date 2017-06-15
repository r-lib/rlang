#ifndef RLANG_SEXP_H
#define RLANG_SEXP_H

#include <stdbool.h>


SEXPTYPE r_typeof(SEXP x);
SEXP r_get_attr(SEXP x, SEXP sym);

void mut_attr(SEXP x, SEXP sym, SEXP attr);
void mut_class(SEXP x, SEXP classes);

SEXP set_attr(SEXP x, SEXP sym, SEXP attr);
SEXP set_class(SEXP x, SEXP classes);

SEXP sxp_class(SEXP x);
SEXP sxp_names(SEXP x);

void mut_names(SEXP x, SEXP nms);
bool is_named(SEXP x);


#endif
