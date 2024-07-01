#ifndef RLANG_VIEW_H
#define RLANG_VIEW_H

r_obj* r_vec_view(r_obj* x, r_ssize start, r_ssize size);

static inline
bool r_view_is_materialized(r_obj* x) {
  return r_altrep_data1(x) == r_null;
}

r_obj* r_view_materialize(r_obj* x);

bool r_is_view(r_obj* x);
void r_check_view(r_obj* x);

#endif
