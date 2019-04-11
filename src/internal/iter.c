#include <rlang.h>


#define ITER_DATA(x) ((struct iter_data*) RAW(x))

struct iter_data {
  double i;
  double n;
  sexp* x;
  sexp* names;
  sexp** names_ptr;
};

sexp* r_new_list_iterator(sexp* x) {
  if (r_typeof(x) != r_type_list) {
    r_abort("Internal error: Expected a list in `r_new_list_iterator()`");
  }

  sexp* iter = KEEP(r_new_vector(r_type_raw, sizeof(struct iter_data)));
  struct iter_data* data = ITER_DATA(iter);

  data->i = 0;
  data->n = r_length(x);
  data->x = x;
  data->names = r_vec_names(x);
  data->names_ptr = (data->names == r_null) ? NULL : r_chr_deref(data->names);;

  FREE(1);

  if ((data->n) == 0) {
    return r_null;
  } else {
    return iter;
  }
}

static bool r_list_iter_advance(sexp* iter) {
  struct iter_data* data = ITER_DATA(iter);

  if ((data->i + 1) >= (data->n)) {
    return false;
  }

  ++(data->i);

  if (data->names_ptr) {
    ++(data->names_ptr);
  }

  return true;
}

sexp* r_list_iter_value(sexp* iter) {
  if (iter == r_null) {
    return r_null;
  }
  struct iter_data* data = ITER_DATA(iter);
  return r_list_get(data->x, data->i);
}
sexp* r_list_iter_name(sexp* iter) {
  if (iter == r_null) {
    return r_null;
  }
  struct iter_data* data = ITER_DATA(iter);
  return (data->names_ptr) ? *(data->names_ptr) : r_null;
}
sexp* r_list_iter_rest(sexp* iter) {
  if (iter == r_null || !r_list_iter_advance(iter)) {
    return r_null;
  } else {
    return iter;
  }
}

void r_list_iter_poke_value(sexp* iter, sexp* value) {
  struct iter_data* data = ITER_DATA(iter);
  r_list_poke(data->x, data->i, value);
}
void r_list_iter_poke_name(sexp* iter, sexp* name) {
  struct iter_data* data = ITER_DATA(iter);

  if (!(data->names_ptr)) {
    sexp* names = KEEP(r_new_vector(r_type_character, data->n));
    r_poke_names(data->x, names);
    FREE(1);
    data->names = names;
    data->names_ptr = r_chr_deref(names);
  }

  // Don't assign directly in memory as the CHARSXP might be newer
  // than `x` or its names
  r_chr_poke(data->names, data->i, name);
}
