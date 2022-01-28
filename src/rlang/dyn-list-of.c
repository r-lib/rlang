#include <rlang.h>
#include "decl/dyn-list-of-decl.h"

#define R_DYN_LOF_GROWTH_FACTOR 2

#define R_DYN_LOF_INIT_SIZE 32

enum shelter_dyn_list_of {
  SHELTER_DYN_LOF_raw,
  SHELTER_DYN_LOF_reserve,
  SHELTER_DYN_LOF_arr_locs,
  SHELTER_DYN_LOF_extra_array,
  SHELTER_DYN_LOF_extra_shelter_array,
  SHELTER_DYN_LOF_moved_arr,
  SHELTER_DYN_LOF_moved_shelter_arr,
  SHELTER_DYN_LOF_arrays,
  SHELTER_DYN_LOF_SIZE
};

struct r_dyn_list_of* r_new_dyn_list_of(enum r_type type,
                                        r_ssize capacity,
                                        r_ssize width) {
  switch (type) {
  case R_TYPE_character:
  case R_TYPE_list:
    r_abort("Can't create a dynamic list of barrier vectors.");
  default:
    break;
  }

  r_obj* shelter = KEEP(r_alloc_list(SHELTER_DYN_LOF_SIZE));

  r_obj* lof_raw = r_alloc_raw(sizeof(struct r_dyn_list_of));
  r_list_poke(shelter, SHELTER_DYN_LOF_raw, lof_raw);

  struct r_dyn_array* p_moved_arr = r_new_dyn_array(sizeof(struct r_dyn_array*), R_DYN_LOF_INIT_SIZE);
  r_list_poke(shelter, SHELTER_DYN_LOF_moved_arr, p_moved_arr->shelter);

  struct r_dyn_array* p_moved_shelter_arr = r_new_dyn_vector(R_TYPE_list, R_DYN_LOF_INIT_SIZE);
  r_list_poke(shelter, SHELTER_DYN_LOF_moved_shelter_arr, p_moved_shelter_arr->shelter);

  r_obj* reserve = r_alloc_vector(type, r_ssize_mult(capacity, width));
  r_list_poke(shelter, SHELTER_DYN_LOF_reserve, reserve);
  void* v_reserve = r_vec_begin(reserve);

  r_obj* arr_locs = r_alloc_raw(sizeof(r_ssize) * capacity);
  r_list_poke(shelter, SHELTER_DYN_LOF_arr_locs, arr_locs);
  r_ssize* v_arr_locs = r_raw_begin(arr_locs);
  R_MEM_SET(r_ssize, v_arr_locs, -1, capacity);

  struct r_dyn_array* p_arrays = r_new_dyn_array(sizeof(struct r_pair_ptr_ssize), capacity);
  r_list_poke(shelter, SHELTER_DYN_LOF_arrays, p_arrays->shelter);

  struct r_dyn_list_of* p_lof = r_raw_begin(lof_raw);
  *p_lof = (struct r_dyn_list_of) {
    .shelter = shelter,
    .count = 0,
    .capacity = capacity,
    .growth_factor = R_DYN_LOF_GROWTH_FACTOR,

    .v_data = r_dyn_begin(p_arrays),

    // private:
    .width = width,
    .type = type,
    .elt_byte_size = r_vec_elt_sizeof0(type),

    .reserve = reserve,
    .v_reserve = v_reserve,

    .p_moved_arr = p_moved_arr,
    .p_moved_shelter_arr = p_moved_shelter_arr,

    .arr_locs = arr_locs,
    .v_arr_locs = v_arr_locs,

    .p_arrays = p_arrays,
  };

  FREE(1);
  return p_lof;
}

r_obj* r_lof_unwrap(struct r_dyn_list_of* p_lof) {
  r_obj* out = KEEP(r_alloc_list(p_lof->count));

  enum r_type type = p_lof->type;
  r_ssize n = p_lof->count;
  struct r_pair_ptr_ssize* v_arrays = r_dyn_begin(p_lof->p_arrays);

  for (r_ssize i = 0; i < n; ++i) {
    struct r_pair_ptr_ssize array = v_arrays[i];
    r_list_poke(out, i, r_vec_n(type, array.ptr, array.size));
  }

  FREE(1);
  return out;
}

static
void r_lof_resize(struct r_dyn_list_of* p_lof, r_ssize capacity) {
  r_ssize count = p_lof->count;

  // Resize reserve
  r_obj* reserve = r_vec_resize0(p_lof->type,
                                 p_lof->reserve,
                                 r_ssize_mult(capacity, p_lof->width));
  r_list_poke(p_lof->shelter, SHELTER_DYN_LOF_reserve, reserve);

  p_lof->reserve = reserve;
  p_lof->v_reserve = r_vec_begin0(p_lof->type, reserve);
  p_lof->capacity = capacity;

  // Resize array indirections
  r_obj* arr_locs = r_raw_resize(p_lof->arr_locs,
                                 r_ssize_mult(sizeof(r_ssize), capacity));
  r_list_poke(p_lof->shelter, SHELTER_DYN_LOF_arr_locs, arr_locs);

  r_ssize* v_arr_locs = r_raw_begin(arr_locs);
  r_ssize n_new = capacity - count;
  R_MEM_SET(r_ssize, v_arr_locs + count, -1, n_new);

  p_lof->arr_locs = arr_locs;
  p_lof->v_arr_locs = v_arr_locs;

  // Resize addresses and update them to point to the new memory
  r_dyn_resize(p_lof->p_arrays, capacity);

  struct r_pair_ptr_ssize* v_data = r_dyn_begin(p_lof->p_arrays);
  p_lof->v_data = v_data;

  unsigned char* v_reserve_u = (unsigned char*) p_lof->v_reserve;
  r_ssize bytes = p_lof->width * p_lof->elt_byte_size;

  for (r_ssize i = 0; i < count; ++i) {
    // Preserve addresses of moved arrays
    if (v_arr_locs[i] < 0) {
      r_ssize offset = i * bytes;
      v_data[i].ptr = v_reserve_u + offset;
    }
  }
}

void r_lof_push_back(struct r_dyn_list_of* p_lof) {
  r_ssize count = p_lof->count + 1;

  if (count > p_lof->capacity) {
    r_ssize new_size = r_ssize_mult(p_lof->capacity, R_DYN_LOF_GROWTH_FACTOR);
    r_lof_resize(p_lof, new_size);
  }
  p_lof->count = count;

  unsigned char* v_reserve_u = (unsigned char*) p_lof->v_reserve;
  r_ssize offset = (count - 1) * p_lof->width * p_lof->elt_byte_size;

  struct r_pair_ptr_ssize info = {
    .ptr = v_reserve_u + offset,
    .size = 0
  };
  r_dyn_push_back(p_lof->p_arrays, &info);
}

void r_lof_arr_push_back(struct r_dyn_list_of* p_lof,
                         r_ssize i,
                         void* p_elt) {
  if (i >= p_lof->count) {
    r_stop_internal("Location %d does not exist.", i);
  }

  if (reserve_push_back(p_lof, i, p_elt)) {
    return;
  }

  struct r_dyn_array* p_arr = p_lof->p_moved_arr;
  r_ssize arr_i = p_lof->v_arr_locs[i];

  if (arr_i >= p_arr->count) {
    r_stop_internal("Location %d does not exist in the extra array", arr_i);
  }

  struct r_dyn_array* p_inner_arr = R_DYN_GET(struct r_dyn_array*, p_arr, arr_i);
  r_dyn_push_back(p_inner_arr, p_elt);

  // Also update pointer in case of resize
  R_DYN_POKE(struct r_pair_ptr_ssize, p_lof->p_arrays, i, ((struct r_pair_ptr_ssize) {
    .ptr = r_dyn_begin(p_inner_arr),
    .size = p_inner_arr->count
  }));
}

static
bool reserve_push_back(struct r_dyn_list_of* p_lof, r_ssize i, void* p_elt) {
  if (p_lof->v_arr_locs[i] >= 0) {
    return false;
  }

  struct r_pair_ptr_ssize* p_arr_info = r_dyn_pointer(p_lof->p_arrays, i);
  if (p_arr_info->size >= p_lof->width) {
    // Inner array is getting too big for the reserve. Move it to a
    // dynamic array.
    reserve_move(p_lof, i, p_elt);
    return false;
  }

  r_ssize count = ++p_arr_info->size;

  r_ssize offset = (i * p_lof->width + count - 1) * p_lof->elt_byte_size;
  void* p = ((unsigned char*) p_lof->v_reserve) + offset;

  if (p_elt) {
    memcpy(p, p_elt, p_lof->elt_byte_size);
  } else {
    memset(p, 0, p_lof->elt_byte_size);
  }

  return true;
}

static
void reserve_move(struct r_dyn_list_of* p_lof, r_ssize i, void* p_elt) {
  struct r_dyn_array* p_moved_arr = p_lof->p_moved_arr;
  r_ssize n = p_lof->width;

  struct r_dyn_array* p_new = r_new_dyn_vector(p_lof->type, p_lof->width);
  r_dyn_list_push_back(p_lof->p_moved_shelter_arr, p_new->shelter);
  r_dyn_push_back(p_moved_arr, &p_new);

  void* v_new = r_dyn_begin(p_new);
  void* v_old = R_DYN_GET(struct r_pair_ptr_ssize, p_lof->p_arrays, i).ptr;
  memcpy(v_new, v_old, r_ssize_mult(n, p_lof->elt_byte_size));

  p_new->count = n;

  R_DYN_POKE(struct r_pair_ptr_ssize, p_lof->p_arrays, i, ((struct r_pair_ptr_ssize) {
    .ptr = v_new,
    .size = n
  }));

  p_lof->v_arr_locs[i] = p_moved_arr->count - 1;
}
