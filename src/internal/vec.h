#ifndef RLANG_INTERNAL_VEC_H
#define RLANG_INTERNAL_VEC_H


bool r_is_vector(sexp* x, r_ssize n);
bool r_is_atomic(sexp* x, r_ssize n);

bool r_is_finite(sexp* x);
bool r_is_logical(sexp* x, r_ssize n);
bool r_is_integerish(sexp* x, r_ssize n, int finite);
bool r_is_integer(sexp* x, r_ssize n, int finite);
bool r_is_double(sexp* x, r_ssize n, int finite);
bool r_is_character(sexp* x, r_ssize n);
bool r_is_raw(sexp* x, r_ssize n);


#endif
