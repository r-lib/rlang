#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H


r_size_t vec_length(SEXP x);

SEXP r_scalar_lgl(bool x);

void r_vec_poke_from(SEXP x, r_size_t offset,
                     SEXP y, r_size_t from, r_size_t to);

void r_vec_poke_coerce_from(SEXP x, r_size_t offset,
                            SEXP y, r_size_t from, r_size_t to);


#endif
