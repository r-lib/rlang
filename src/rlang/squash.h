#ifndef RLANG_SQUASH_H
#define RLANG_SQUASH_H


bool r_is_spliced(SEXP x);
bool r_is_spliced_bare(SEXP x);
SEXP r_squash_if(SEXP dots, SEXPTYPE kind, bool (*is_spliceable)(SEXP), int depth);


#endif
