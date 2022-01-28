#include "rlang.h"
#include <math.h>
#include <stdint.h>


r_obj* r_chr_n(const char* const * strings, r_ssize n) {
  r_obj* out = KEEP(r_alloc_character(n));

  for (r_ssize i = 0; i < n; ++i) {
    r_chr_poke(out, i, r_str(strings[i]));
  }

  FREE(1);
  return out;
}

#if R_VERSION >= R_Version(3, 4, 0)
#define HAS_VIRTUAL_SIZE 1
#else
#define HAS_VIRTUAL_SIZE 0
#endif

#define RESIZE(R_TYPE, C_TYPE, CONST_DEREF, DEREF)              \
  do {                                                          \
    r_ssize x_size = r_length(x);                               \
    if (x_size == size) {                                       \
      return x;                                                 \
    }                                                           \
    if (!ALTREP(x) && size < x_size && HAS_VIRTUAL_SIZE) {      \
      SETLENGTH(x, size);                                       \
      SET_TRUELENGTH(x, x_size);                                \
      SET_GROWABLE_BIT(x);                                      \
      return x;                                                 \
    }                                                           \
                                                                \
    const C_TYPE* p_x = CONST_DEREF(x);                         \
    r_obj* out = KEEP(r_alloc_vector(R_TYPE, size));            \
    C_TYPE* p_out = DEREF(out);                                 \
                                                                \
    r_ssize cpy_size = (size > x_size) ? x_size : size;         \
    memcpy(p_out, p_x, cpy_size * sizeof(C_TYPE));              \
                                                                \
    FREE(1);                                                    \
    return out;                                                 \
  } while (0)

#define RESIZE_BARRIER(R_TYPE, CONST_DEREF, SET)                \
  do {                                                          \
    r_ssize x_size = r_length(x);                               \
    if (x_size == size) {                                       \
      return x;                                                 \
    }                                                           \
    if (!ALTREP(x) && size < x_size && HAS_VIRTUAL_SIZE) {      \
      SETLENGTH(x, size);                                       \
      SET_TRUELENGTH(x, x_size);                                \
      SET_GROWABLE_BIT(x);                                      \
      return x;                                                 \
    }                                                           \
                                                                \
    r_obj* const * p_x = CONST_DEREF(x);                        \
    r_obj* out = KEEP(r_alloc_vector(R_TYPE, size));            \
                                                                \
    r_ssize cpy_size = (size > x_size) ? x_size : size;         \
    for (r_ssize i = 0; i < cpy_size; ++i) {                    \
      SET(out, i, p_x[i]);                                      \
    }                                                           \
                                                                \
    FREE(1);                                                    \
    return out;                                                 \
  } while (0)

// Compared to `Rf_xlengthgets()` this does not initialise the new
// extended locations with `NA`
r_obj* r_lgl_resize(r_obj* x, r_ssize size) {
  RESIZE(R_TYPE_logical, int, r_lgl_cbegin, r_lgl_begin);
}
r_obj* r_int_resize(r_obj* x, r_ssize size) {
  RESIZE(R_TYPE_integer, int, r_int_cbegin, r_int_begin);
}
r_obj* r_dbl_resize(r_obj* x, r_ssize size) {
  RESIZE(R_TYPE_double, double, r_dbl_cbegin, r_dbl_begin);
}
r_obj* r_cpl_resize(r_obj* x, r_ssize size) {
  RESIZE(R_TYPE_complex, r_complex, r_cpl_cbegin, r_cpl_begin);
}
r_obj* r_raw_resize(r_obj* x, r_ssize size) {
  RESIZE(R_TYPE_raw, unsigned char, r_raw_cbegin, r_raw_begin);
}
r_obj* r_chr_resize(r_obj* x, r_ssize size) {
  RESIZE_BARRIER(R_TYPE_character, r_chr_cbegin, r_chr_poke);
}
r_obj* r_list_resize(r_obj* x, r_ssize size) {
  RESIZE_BARRIER(R_TYPE_list, r_list_cbegin, r_list_poke);
}

#undef RESIZE
#undef RESIZE_BARRIER


r_obj* r_list_compact(r_obj* x) {
  r_ssize n = r_length(x);
  r_obj* inc = KEEP(r_alloc_logical(n));

  int* v_inc = r_int_begin(inc);
  r_obj* const * v_x = r_list_cbegin(x);

  r_ssize new_n = 0;
  for (r_ssize i = 0; i < n; ++i) {
    v_inc[i] = v_x[i] != r_null;
    new_n += v_inc[i];
  }

  r_obj* out = KEEP(r_alloc_list(new_n));
  for (r_ssize i = 0, count = 0; i < n; ++i) {
    if (v_inc[i]) {
      r_list_poke(out, count, v_x[i]);
      ++count;
    }
  }

  FREE(2);
  return out;
}

r_obj* r_list_of_as_ptr_ssize(r_obj* xs,
                              enum r_type type,
                              struct r_pair_ptr_ssize** p_v_out) {
  if (r_typeof(xs) != R_TYPE_list) {
    r_abort("`xs` must be a list.");
  }
  r_ssize n = r_length(xs);

  r_obj* shelter = KEEP(r_alloc_raw(sizeof(struct r_pair_ptr_ssize) * n));
  struct r_pair_ptr_ssize* v_out = r_raw_begin(shelter);

  r_obj* const * v_xs = r_list_cbegin(xs);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];
    if (r_typeof(x) != type) {
      r_abort("`xs` must be a list of vectors of type `%s`.",
              r_type_as_c_string(type));
    }

    v_out[i] = (struct r_pair_ptr_ssize) {
      .ptr = r_int_begin(x),
      .size = r_length(x)
    };
  }

  FREE(1);
  *p_v_out = v_out;
  return shelter;
}


// FIXME: Does this have a place in the library?
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n) {

  if ((r_length(x) - offset) < n) {
    r_abort("Can't copy data to `x` because it is too small");
  }
  if ((r_length(y) - from) < n) {
    r_abort("Can't copy data from `y` because it is too small");
  }

  switch (r_typeof(x)) {
  case R_TYPE_logical: {
    int* src_data = r_lgl_begin(y);
    int* dest_data = r_lgl_begin(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case R_TYPE_integer: {
    int* src_data = r_int_begin(y);
    int* dest_data = r_int_begin(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case R_TYPE_double: {
    double* src_data = r_dbl_begin(y);
    double* dest_data = r_dbl_begin(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case R_TYPE_complex: {
    r_complex* src_data = r_cpl_begin(y);
    r_complex* dest_data = r_cpl_begin(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case R_TYPE_raw: {
    unsigned char* src_data = RAW(y);
    unsigned char* dest_data = RAW(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case R_TYPE_character: {
    r_obj* elt;
    for (r_ssize i = 0; i != n; ++i) {
      elt = r_chr_get(y, i + from);
      r_chr_poke(x, i + offset, elt);
    }
    break;
  }
  case R_TYPE_list: {
    r_obj* elt;
    for (r_ssize i = 0; i != n; ++i) {
      elt = r_list_get(y, i + from);
      r_list_poke(x, i + offset, elt);
    }
    break;
  }
  default:
    r_abort("Copy requires vectors");
  }
}

void r_vec_poke_range(r_obj* x, r_ssize offset,
                      r_obj* y, r_ssize from, r_ssize to) {
  r_vec_poke_n(x, offset, y, from, to - from + 1);
}

bool _r_is_finite(r_obj* x) {
  r_ssize n = r_length(x);

  switch(r_typeof(x)) {
  case R_TYPE_integer: {
    const int* p_x = r_int_cbegin(x);
    for (r_ssize i = 0; i < n; ++i) {
      if (p_x[i] == r_globals.na_int) {
        return false;
      }
    }
    break;
  }
  case R_TYPE_double: {
    const double* p_x = r_dbl_cbegin(x);
    for (r_ssize i = 0; i < n; ++i) {
      if (!isfinite(p_x[i])) {
        return false;
      }
    }
    break;
  }
  case R_TYPE_complex: {
    const r_complex* p_x = r_cpl_cbegin(x);
    for (r_ssize i = 0; i < n; ++i) {
      if (!isfinite(p_x[i].r) || !isfinite(p_x[i].i)) {
        return false;
      }
    }
    break;
  }
  default:
    r_abort("Internal error: expected a numeric vector");
  }

  return true;
}
