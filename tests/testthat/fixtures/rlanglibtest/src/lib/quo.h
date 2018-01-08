#ifndef RLANG_QUO_H
#define RLANG_QUO_H


extern sexp* (*r_quo_get_expr)(sexp* quo);
extern sexp* (*r_quo_set_expr)(sexp* quo, sexp* expr);
extern sexp* (*r_quo_get_env)(sexp* quo);
extern sexp* (*r_quo_set_env)(sexp* quo, sexp* env);


#endif
