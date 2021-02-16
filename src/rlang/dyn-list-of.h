#ifndef RLANG_DYN_DYN_LIST_OF_H
#define RLANG_DYN_DYN_LIST_OF_H


struct r_dyn_list_of {
  sexp* shelter;
  r_ssize count;
  r_ssize capacity;
  int growth_factor;

  struct r_pair_ptr_ssize* v_arrays;
  r_ssize* v_capacities;

  // private:
  sexp* reserve;
  void* v_reserve;

  enum r_type type;
  r_ssize elt_byte_size;
};

struct r_dyn_list_of* r_new_dyn_list_of(enum r_type type,
                                        r_ssize capacity,
                                        r_ssize arr_capacities);


#endif
