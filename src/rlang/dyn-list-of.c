#include <rlang.h>

#define R_DYN_LOF_GROWTH_FACTOR 2

enum shelter_dyn_list_of {
  SHELTER_DYN_LOF_raw,
  SHELTER_DYN_LOF_reserve,
  SHELTER_DYN_LOF_arrays,
  SHELTER_DYN_LOF_capacities,
  SHELTER_DYN_LOF_SIZE
};

struct r_dyn_list_of* r_new_dyn_list_of(enum r_type type,
                                        r_ssize capacity,
                                        r_ssize arr_capacities) {
  switch (type) {
  case r_type_character:
  case r_type_list:
    r_abort("Can't create a dynamic list of barrier vectors.");
  default:
    break;
  }

  sexp* shelter = KEEP(r_new_list(SHELTER_DYN_LOF_SIZE));

  sexp* lof_raw = r_new_raw(sizeof(struct r_dyn_list_of));
  r_list_poke(shelter, SHELTER_DYN_LOF_raw, lof_raw);

  r_ssize reserve_size = r_ssize_mult(capacity, arr_capacities);

  sexp* reserve = r_new_vector(type, reserve_size);
  r_list_poke(shelter, SHELTER_DYN_LOF_reserve, reserve);

  sexp* arrays = r_new_raw(r_ssize_mult(sizeof(struct r_pair_ptr_ssize), capacity));
  r_list_poke(shelter, SHELTER_DYN_LOF_arrays, arrays);

  sexp* capacities = r_new_raw(r_ssize_mult(sizeof(r_ssize), capacity));
  r_list_poke(shelter, SHELTER_DYN_LOF_capacities, capacities);

  struct r_pair_ptr_ssize* v_arrays = r_raw_deref(arrays);
  r_ssize* v_capacities = r_raw_deref(capacities);

  for (r_ssize i = 0; i < capacity; ++i) {
    r_ssize offset = i * arr_capacities;

    v_arrays[i] = (struct r_pair_ptr_ssize) {
      .ptr = v_arrays + offset,
      .size = 0
    };
    v_capacities[i] = arr_capacities;
  }

  struct r_dyn_list_of* p_lof = r_raw_deref(lof_raw);
  *p_lof = (struct r_dyn_list_of) {
    .shelter = shelter,
    .count = 0,
    .capacity = capacity,
    .growth_factor = R_DYN_LOF_GROWTH_FACTOR,
    .v_arrays = v_arrays,
    .v_capacities = v_capacities,
    .type = type,
    .elt_byte_size = r_vec_elt_sizeof0(type),
    .reserve = reserve,
    .v_reserve = r_vec_deref(reserve)
  };

  FREE(1);
  return p_lof;
}


// TODO -------------------------------------------------------------------

void r_lof_resize(struct r_dyn_list_of* p_lof,
                  r_ssize capacity);

void r_lof_push_back(struct r_dyn_list_of* p_lof,
                     void* p_elt,
                     r_ssize size);

void r_lof_arr_push_back(struct r_dyn_list_of* p_lof,
                         r_ssize i,
                         void* p_elt);

sexp* r_lof_unwrap(struct r_dyn_list_of* p_lof);
