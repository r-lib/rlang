#ifndef RLANG_DYN_LIST_OF_H
#define RLANG_DYN_LIST_OF_H


struct r_dyn_list_of {
  sexp* shelter;
  r_ssize count;
  r_ssize capacity;
  int growth_factor;

  // Dynamic array of `struct r_pair_ptr_ssize` containing the
  // addresses and sizes of each element of the list-of
  struct r_dyn_array* p_arrays;

  // private:
  r_ssize width;

  sexp* data;
  void* v_data;
  r_ssize* v_data_arr_locs;

  struct r_dyn_array* p_extra_array;
  struct r_dyn_array* p_extra_shelter_array;

  struct r_dyn_array* p_moved_arr;
  struct r_dyn_array* p_moved_shelter_arr;

  enum r_type type;
  r_ssize elt_byte_size;
};

struct r_dyn_list_of* r_new_dyn_list_of(enum r_type type,
                                        r_ssize capacity,
                                        r_ssize width);

sexp* r_lof_unwrap(struct r_dyn_list_of* p_lof);

void r_lof_push_back(struct r_dyn_list_of* p_lof);

void r_lof_arr_push_back(struct r_dyn_list_of* p_lof,
                         r_ssize i,
                         void* p_elt);

static inline
void* r_lof_arr_ptr(struct r_dyn_list_of* p_lof, r_ssize i, r_ssize j) {
  r_ssize offset = j * p_lof->elt_byte_size;
  struct r_pair_ptr_ssize* v_arrays = r_arr_ptr(p_lof->p_arrays, i);
  return ((unsigned char*) v_arrays->ptr) + offset;
}
static inline
void* r_lof_arr_ptr_front(struct r_dyn_list_of* p_lof, r_ssize i) {
  return r_lof_arr_ptr(p_lof, i, 0);
}
static inline
void* r_lof_arr_ptr_back(struct r_dyn_list_of* p_lof, r_ssize i) {
  return r_lof_arr_ptr(p_lof, i, p_lof->count - 1);
}


#endif
