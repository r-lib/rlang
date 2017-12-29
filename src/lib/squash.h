#ifndef RLANG_SQUASH_H
#define RLANG_SQUASH_H


bool r_is_spliced(sexp* x);
bool r_is_spliced_bare(sexp* x);
sexp* r_squash_if(sexp* dots, enum r_type kind, bool (*is_spliceable)(sexp*), int depth);


#endif
