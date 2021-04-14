#ifndef RLANG_INTERNAL_VEC_H
#define RLANG_INTERNAL_VEC_H


bool r_is_vector(r_obj* x, r_ssize n);
bool r_is_atomic(r_obj* x, r_ssize n);

bool r_is_finite(r_obj* x);
bool r_is_logical(r_obj* x, r_ssize n);
bool r_is_integerish(r_obj* x, r_ssize n, int finite);
bool r_is_integer(r_obj* x, r_ssize n, int finite);
bool r_is_double(r_obj* x, r_ssize n, int finite);
bool r_is_complex(r_obj* x, r_ssize n, int finite);
bool r_is_character(r_obj* x, r_ssize n);
bool r_is_raw(r_obj* x, r_ssize n);

void r_vec_poke_coerce_n(r_obj* x, r_ssize offset,
                         r_obj* y, r_ssize from, r_ssize n);
void r_vec_poke_coerce_range(r_obj* x, r_ssize offset,
                             r_obj* y, r_ssize from, r_ssize to);


#endif
