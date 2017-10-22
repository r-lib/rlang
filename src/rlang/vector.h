#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H


r_size_t r_vec_length(SEXP x);

bool r_is_list(SEXP x);
bool r_is_vector(SEXP x);
bool r_is_scalar_atomic(SEXP x);
bool r_is_atomic(SEXP x);

SEXP r_scalar_lgl(bool x);

void r_vec_poke_range(SEXP x, r_size_t offset,
                      SEXP y, r_size_t from, r_size_t to);

void r_vec_poke_coerce_range(SEXP x, r_size_t offset,
                             SEXP y, r_size_t from, r_size_t to);


#endif
