#ifndef RLANG_INTERNAL_VEC_H
#define RLANG_INTERNAL_VEC_H


enum option_bool {
  OPTION_BOOL_false = -1,
  OPTION_BOOL_null = 0,
  OPTION_BOOL_true = 1
};

bool r_is_vector(r_obj* x, r_ssize n);
bool r_is_atomic(r_obj* x, r_ssize n);

bool _r_is_finite(r_obj* x);
bool r_is_logical(r_obj* x, r_ssize n);
bool r_is_integerish(r_obj* x, r_ssize n, int finite);
bool r_is_integer(r_obj* x, r_ssize n, int finite);
bool r_is_double(r_obj* x, r_ssize n, int finite);
bool r_is_complex(r_obj* x, r_ssize n, int finite);
bool is_character(r_obj* x,
                  r_ssize n,
                  enum option_bool missing,
                  enum option_bool empty);
bool r_is_raw(r_obj* x, r_ssize n);

r_ssize validate_n(r_obj* n);

void r_vec_poke_coerce_n(r_obj* x, r_ssize offset,
                         r_obj* y, r_ssize from, r_ssize n);
void r_vec_poke_coerce_range(r_obj* x, r_ssize offset,
                             r_obj* y, r_ssize from, r_ssize to);


#endif
