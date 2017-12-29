#ifndef RLANG_QUO_H
#define RLANG_QUO_H


sexp* rlang_new_quosure(sexp* expr, sexp* env);
bool rlang_is_quosure(sexp* x);

sexp* rlang_get_expression(sexp* x, sexp* alternate);


void check_quosure(sexp* x);
bool quo_is_missing(sexp* quo);
bool quo_is_symbol(sexp* quo);
bool quo_is_call(sexp* quo);
bool quo_is_symbolic(sexp* quo);
bool quo_is_null(sexp* quo);


#endif
