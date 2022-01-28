#ifndef RLANG_DYN_ARRAY_H
#define RLANG_DYN_ARRAY_H

#include "vec.h"


struct r_dyn_array {
  r_obj* shelter;
  r_ssize count;
  r_ssize capacity;
  int growth_factor;

  r_obj* data;
  void* v_data;
  const void* v_data_const;

  // private:
  enum r_type type;
  r_ssize elt_byte_size;
  void (*barrier_set)(r_obj* x, r_ssize i, r_obj* value);
};

struct r_dyn_array* r_new_dyn_vector(enum r_type type,
                                     r_ssize capacity);

struct r_dyn_array* r_new_dyn_array(r_ssize elt_byte_size,
                                    r_ssize capacity);

void r_dyn_resize(struct r_dyn_array* p_arr,
                  r_ssize capacity);

void r_dyn_push_back(struct r_dyn_array* p_arr,
                     const void* p_elt);

r_obj* r_dyn_unwrap(struct r_dyn_array* p_arr);

static inline
void* r_dyn_pointer(struct r_dyn_array* p_arr, r_ssize i) {
  if (p_arr->barrier_set) {
    r_abort("Can't take mutable pointer of barrier vector.");
  }
  r_ssize offset = i * p_arr->elt_byte_size;
  return ((unsigned char*) p_arr->v_data) + offset;
}
static inline
void* r_dyn_begin(struct r_dyn_array* p_arr) {
  return r_dyn_pointer(p_arr, 0);
}
static inline
void* r_dyn_last(struct r_dyn_array* p_arr) {
  return r_dyn_pointer(p_arr, p_arr->count - 1);
}
static inline
void* r_dyn_end(struct r_dyn_array* p_arr) {
  return r_dyn_pointer(p_arr, p_arr->count);
}

static inline
const void* r_dyn_cpointer(struct r_dyn_array* p_arr, r_ssize i) {
  r_ssize offset = i * p_arr->elt_byte_size;
  return ((const unsigned char*) p_arr->v_data_const) + offset;
}
static inline
const void* r_dyn_cbegin(struct r_dyn_array* p_arr) {
  return r_dyn_cpointer(p_arr, 0);
}
static inline
const void* r_dyn_clast(struct r_dyn_array* p_arr) {
  return r_dyn_cpointer(p_arr, p_arr->count - 1);
}
static inline
const void* r_dyn_cend(struct r_dyn_array* p_arr) {
  return r_dyn_cpointer(p_arr, p_arr->count);
}

static inline
void* const * r_dyn_pop_back(struct r_dyn_array* p_arr) {
  void* const * out = (void* const *) r_dyn_clast(p_arr);
  --p_arr->count;
  return out;
}

static inline
void r_dyn_lgl_push_back(struct r_dyn_array* p_vec, int elt) {
  r_dyn_push_back(p_vec, &elt);
}
static inline
void r_dyn_int_push_back(struct r_dyn_array* p_vec, int elt) {
  r_dyn_push_back(p_vec, &elt);
}
static inline
void r_dyn_dbl_push_back(struct r_dyn_array* p_vec, double elt) {
  r_dyn_push_back(p_vec, &elt);
}
static inline
void r_dyn_cpl_push_back(struct r_dyn_array* p_vec, r_complex elt) {
  r_dyn_push_back(p_vec, &elt);
}
static inline
void r_dyn_list_push_back(struct r_dyn_array* p_vec, r_obj* elt) {
  KEEP(elt);
  r_dyn_push_back(p_vec, &elt);
  FREE(1);
}

#define R_DYN_GET(TYPE, X, I) (*((TYPE*) r_dyn_pointer((X), (I))))
#define R_DYN_POKE(TYPE, X, I, VAL) (*((TYPE*) r_dyn_pointer((X), (I))) = (VAL))

static inline
int r_dyn_lgl_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((const int*) p_vec->v_data_const)[i];
}
static inline
int r_dyn_int_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((const int*) p_vec->v_data_const)[i];
}
static inline
double r_dyn_dbl_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((const double*) p_vec->v_data_const)[i];
}
static inline
r_complex r_dyn_cpl_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((const r_complex*) p_vec->v_data_const)[i];
}
static inline
char r_dyn_raw_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((const char*) p_vec->v_data_const)[i];
}
static inline
r_obj* r_dyn_chr_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((r_obj* const *) p_vec->v_data_const)[i];
}
static inline
r_obj* r_dyn_list_get(struct r_dyn_array* p_vec, r_ssize i) {
  return ((r_obj* const *) p_vec->v_data_const)[i];
}

static inline
void r_dyn_lgl_poke(struct r_dyn_array* p_vec, r_ssize i, int value) {
  ((int*) p_vec->v_data)[i] = value;
}
static inline
void r_dyn_int_poke(struct r_dyn_array* p_vec, r_ssize i, int value) {
  ((int*) p_vec->v_data)[i] = value;
}
static inline
void r_dyn_dbl_poke(struct r_dyn_array* p_vec, r_ssize i, double value) {
  ((double*) p_vec->v_data)[i] = value;
}
static inline
void r_dyn_cpl_poke(struct r_dyn_array* p_vec, r_ssize i, r_complex value) {
  ((r_complex*) p_vec->v_data)[i] = value;
}
static inline
void r_dyn_raw_poke(struct r_dyn_array* p_vec, r_ssize i, char value) {
  ((char*) p_vec->v_data)[i] = value;
}
static inline
void r_dyn_chr_poke(struct r_dyn_array* p_vec, r_ssize i, r_obj* value) {
  r_chr_poke(p_vec->data, i, value);
}
static inline
void r_dyn_list_poke(struct r_dyn_array* p_vec, r_ssize i, r_obj* value) {
  r_list_poke(p_vec->data, i, value);
}

#endif
