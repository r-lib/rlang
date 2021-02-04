#ifndef RLANG_DYN_ARRAY_H
#define RLANG_DYN_ARRAY_H


struct r_dyn_array {
  sexp* shelter;
  r_ssize count;
  r_ssize capacity;
  int growth_factor;

  sexp* data;
  void* v_data;
  const void* v_data_const;

  // private:
  enum r_type type;
  r_ssize elt_byte_size;
  void (*barrier_set)(sexp* x, r_ssize i, sexp* value);
};

struct r_dyn_array* r_new_dyn_vector(enum r_type type,
                                     r_ssize capacity);

struct r_dyn_array* r_new_dyn_array(r_ssize capacity,
                                    r_ssize elt_byte_size);

void r_arr_resize(struct r_dyn_array* p_arr,
                  r_ssize capacity);

void r_arr_push_back(struct r_dyn_array* p_arr, void* p_elt);

static inline
void r_arr_pop_back(struct r_dyn_array* p_arr) {
  --p_arr->count;
}

static inline
void* r_arr_ptr(struct r_dyn_array* p_arr, r_ssize i) {
  if (p_arr->barrier_set) {
    r_abort("Can't take mutable pointer of barrier vector.");
  }
  r_ssize offset = i * p_arr->elt_byte_size;
  return ((unsigned char*) p_arr->v_data) + offset;
}
static inline
void* r_arr_ptr_front(struct r_dyn_array* p_arr) {
  return r_arr_ptr(p_arr, 0);
}
static inline
void* r_arr_ptr_back(struct r_dyn_array* p_arr) {
  return r_arr_ptr(p_arr, p_arr->count - 1);
}
static inline
void* r_arr_ptr_end(struct r_dyn_array* p_arr) {
  return r_arr_ptr(p_arr, p_arr->count);
}

static inline
const void* r_arr_ptr_const(struct r_dyn_array* p_arr, r_ssize i) {
  r_ssize offset = i * p_arr->elt_byte_size;
  return ((const unsigned char*) p_arr->v_data) + offset;
}
static inline
const void* r_arr_ptr_const_front(struct r_dyn_array* p_arr) {
  return r_arr_ptr_const(p_arr, 0);
}
static inline
const void* r_arr_ptr_const_back(struct r_dyn_array* p_arr) {
  return r_arr_ptr_const(p_arr, p_arr->count - 1);
}
static inline
const void* r_arr_ptr_const_end(struct r_dyn_array* p_arr) {
  return r_arr_ptr_const(p_arr, p_arr->count);
}


static inline
void r_lgl_push_back(struct r_dyn_array* p_vec, int elt) {
  r_arr_push_back(p_vec, &elt);
}
static inline
void r_int_push_back(struct r_dyn_array* p_vec, int elt) {
  r_arr_push_back(p_vec, &elt);
}
static inline
void r_dbl_push_back(struct r_dyn_array* p_vec, double elt) {
  r_arr_push_back(p_vec, &elt);
}
static inline
void r_cpl_push_back(struct r_dyn_array* p_vec, r_complex_t elt) {
  r_arr_push_back(p_vec, &elt);
}


#endif
