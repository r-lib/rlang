#ifndef RLANG_INTERNAL_SQUASH_H
#define RLANG_INTERNAL_SQUASH_H


sexp* r_squash_if(sexp* dots, enum r_type kind, bool (*is_spliceable)(sexp*), int depth);


#endif
