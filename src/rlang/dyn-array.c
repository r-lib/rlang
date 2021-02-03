#include <rlang.h>
#include "dyn-array.h"

#define R_DYN_ARRAY_GROWTH_FACTOR 2

static
sexp* attribs_dyn_array = NULL;


struct r_dyn_array* r_new_dyn_array(r_ssize capacity,
                                    r_ssize elt_byte_size) {
  sexp* shelter = KEEP(r_new_list(2));
  r_poke_attrib(shelter, attribs_dyn_array);
  r_mark_object(shelter);

  sexp* vec_raw = r_new_raw(sizeof(struct r_dyn_array));
  r_list_poke(shelter, 0, vec_raw);

  sexp* arr_raw = r_new_raw(r_ssize_mult(capacity, elt_byte_size));
  r_list_poke(shelter, 1, arr_raw);

  struct r_dyn_array* p_arr = r_raw_deref(vec_raw);
  p_arr->shelter = shelter;
  p_arr->count = 0;
  p_arr->capacity = capacity;
  p_arr->growth_factor = R_DYN_ARRAY_GROWTH_FACTOR;
  p_arr->v_data = r_raw_deref(arr_raw);
  p_arr->elt_byte_size = elt_byte_size;

  FREE(1);
  return p_arr;
}

void r_arr_push_back(struct r_dyn_array* p_arr, void* p_elt) {
  r_ssize count = ++p_arr->count;
  r_arr_grow(p_arr, count);

  if (p_elt) {
    memcpy(r_arr_ptr_back(p_arr), p_elt, p_arr->elt_byte_size);
  } else {
    memset(r_arr_ptr_back(p_arr), 0, p_arr->elt_byte_size);
  }
}

void r_arr_grow(struct r_dyn_array* p_arr,
                r_ssize capacity) {
  if (capacity <= p_arr->capacity) {
    return;
  }

  r_ssize new_capacity = r_ssize_mult(p_arr->capacity,
                                      p_arr->growth_factor);
  r_arr_resize(p_arr, new_capacity);
}

void r_arr_resize(struct r_dyn_array* p_arr,
                  r_ssize capacity) {
  sexp* arr_raw = r_raw_resize(r_list_get(p_arr->shelter, 1),
                               p_arr->elt_byte_size * capacity);
  r_list_poke(p_arr->shelter, 1, arr_raw);

  p_arr->count = r_ssize_min(p_arr->count, capacity);
  p_arr->capacity = capacity;
  p_arr->v_data = r_raw_deref(arr_raw);
}


void r_init_library_dyn_array() {
  attribs_dyn_array = r_preserve_global(r_pairlist(r_chr("rlang_dyn_array")));
  r_node_poke_tag(attribs_dyn_array, r_syms_class);
}
