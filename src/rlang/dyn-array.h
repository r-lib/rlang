#ifndef RLANG_DYN_ARRAY_H
#define RLANG_DYN_ARRAY_H


struct r_dyn_array {
  sexp* shelter;
  r_ssize count;
  r_ssize capacity;
  int growth_factor;
  void* v_data;
  r_ssize elt_byte_size;
};

struct r_dyn_array* r_new_dyn_array(r_ssize capacity,
                                    r_ssize elt_byte_size);

void r_arr_resize(struct r_dyn_array* p_arr,
                  r_ssize capacity);

void r_arr_grow(struct r_dyn_array* p_arr,
                r_ssize capacity);

void r_arr_push_back(struct r_dyn_array* p_arr, void* p_elt);

static inline
void r_arr_pop_back(struct r_dyn_array* p_arr) {
  --p_arr->count;
}

static inline
void* r_arr_ptr(struct r_dyn_array* p_arr, r_ssize i) {
  r_ssize offset = i * p_arr->elt_byte_size;
  return ((unsigned char*) p_arr->v_data) + offset;
}
static inline
void* r_arr_ptr_front(struct r_dyn_array* p_arr) {
  return p_arr->v_data;
}
static inline
void* r_arr_ptr_back(struct r_dyn_array* p_arr) {
  return r_arr_ptr(p_arr, p_arr->count - 1);
}
static inline
void* r_arr_ptr_end(struct r_dyn_array* p_arr) {
  return r_arr_ptr(p_arr, p_arr->count);
}



#endif
