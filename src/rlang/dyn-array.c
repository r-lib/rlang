#include <rlang.h>
#include "dyn-array.h"

#define R_DYN_ARRAY_GROWTH_FACTOR 2

static
r_obj* attribs_dyn_array = NULL;


struct r_dyn_array* r_new_dyn_vector(enum r_type type,
                                     r_ssize capacity) {
  r_obj* shelter = KEEP(r_alloc_list(2));
  r_poke_attrib(shelter, attribs_dyn_array);
  r_mark_object(shelter);

  r_obj* vec_raw = r_alloc_raw(sizeof(struct r_dyn_array));
  r_list_poke(shelter, 0, vec_raw);

  r_obj* vec_data = r_alloc_vector(type, capacity);
  r_list_poke(shelter, 1, vec_data);

  struct r_dyn_array* p_vec = r_raw_begin(vec_raw);
  p_vec->shelter = shelter;
  p_vec->count = 0;
  p_vec->capacity = capacity;
  p_vec->growth_factor = R_DYN_ARRAY_GROWTH_FACTOR;
  p_vec->type = type;
  p_vec->elt_byte_size = r_vec_elt_sizeof0(type);
  p_vec->data = vec_data;

  switch (type) {
  case R_TYPE_character:
    p_vec->v_data = NULL;
    p_vec->barrier_set = &r_chr_poke;
    break;
  case R_TYPE_list:
    p_vec->v_data = NULL;
    p_vec->barrier_set = &r_list_poke;
    break;
  default:
    p_vec->barrier_set = NULL;
    p_vec->v_data = r_vec_begin0(type, vec_data);
    break;
  }
  p_vec->v_data_const = r_vec_cbegin0(type, vec_data);

  FREE(1);
  return p_vec;
}

r_obj* r_dyn_unwrap(struct r_dyn_array* p_arr) {
  if (p_arr->type == R_TYPE_raw) {
    return r_raw_resize(p_arr->data, p_arr->count * p_arr->elt_byte_size);
  } else {
    return r_vec_resize0(p_arr->type, p_arr->data, p_arr->count);
  }
}

struct r_dyn_array* r_new_dyn_array(r_ssize elt_byte_size,
                                    r_ssize capacity) {
  r_ssize arr_byte_size = r_ssize_mult(capacity, elt_byte_size);

  struct r_dyn_array* p_arr = r_new_dyn_vector(R_TYPE_raw, arr_byte_size);
  p_arr->capacity = capacity;
  p_arr->elt_byte_size = elt_byte_size;

  return p_arr;
}


void r_dyn_push_back(struct r_dyn_array* p_arr,
                     const void* p_elt) {
  r_ssize count = ++p_arr->count;
  if (count > p_arr->capacity) {
    r_ssize new_capacity = r_ssize_mult(p_arr->capacity,
                                        p_arr->growth_factor);
    r_dyn_resize(p_arr, new_capacity);
  }

  if (p_arr->barrier_set) {
    r_obj* value = *((r_obj* const *) p_elt);
    p_arr->barrier_set(p_arr->data, count - 1, value);
    return;
  }

  if (p_elt) {
    memcpy(r_dyn_last(p_arr), p_elt, p_arr->elt_byte_size);
  } else {
    memset(r_dyn_last(p_arr), 0, p_arr->elt_byte_size);
  }
}

void r_dyn_resize(struct r_dyn_array* p_arr,
                  r_ssize capacity) {
  enum r_type type = p_arr->type;

  r_ssize capacity_multiplier = p_arr->type == R_TYPE_raw ?
    r_ssize_mult(p_arr->elt_byte_size, capacity) :
    capacity;

  r_obj* data = r_vec_resize0(type,
                              r_list_get(p_arr->shelter, 1),
                              capacity_multiplier);
  r_list_poke(p_arr->shelter, 1, data);

  p_arr->count = r_ssize_min(p_arr->count, capacity);
  p_arr->capacity = capacity;
  p_arr->data = data;

  switch (type) {
  case R_TYPE_character:
  case R_TYPE_list:
    break;
  default:
    p_arr->v_data = r_vec_begin0(type, data);
    break;
  }
  p_arr->v_data_const = r_vec_cbegin0(type, data);
}


void r_init_library_dyn_array() {
  r_preserve_global(attribs_dyn_array = r_pairlist(r_chr("rlang_dyn_array")));
  r_node_poke_tag(attribs_dyn_array, r_syms.class_);
}
