#ifndef RLANG_INTERNAL_ITER_H
#define RLANG_INTERNAL_ITER_H


sexp* r_new_list_iterator(sexp* x);
sexp* r_list_iter_value(sexp* iter);
sexp* r_list_iter_name(sexp* iter);
sexp* r_list_iter_rest(sexp* iter);
void r_list_iter_poke_value(sexp* iter, sexp* value);
void r_list_iter_poke_name(sexp* iter, sexp* name);


#endif
