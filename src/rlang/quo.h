#ifndef RLANG_QUO_H
#define RLANG_QUO_H


SEXP r_new_quosure(SEXP expr, SEXP env);
bool r_quo_is_missing(SEXP x);


#endif
