#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

#include "rlang.h"


R_len_t vec_length(SEXP x);

void vec_copy_n(SEXP src, R_len_t n, SEXP dest,
                R_len_t offset_dest,
                R_len_t offset_src);

void vec_copy_coerce_n(SEXP src, R_len_t n, SEXP dest,
                       R_len_t offset_dest,
                       R_len_t offset_src);


#endif
