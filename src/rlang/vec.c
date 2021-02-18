#include "rlang.h"
#include <math.h>
#include <stdint.h>

sexp* r_lgls_empty = NULL;
sexp* r_ints_empty = NULL;
sexp* r_dbls_empty = NULL;
sexp* r_cpls_empty = NULL;
sexp* r_chrs_empty = NULL;
sexp* r_raws_empty = NULL;
sexp* r_lists_empty = NULL;

sexp* r_chrs_empty_string = NULL;
sexp* r_strs_empty = NULL;

sexp* r_true = NULL;
sexp* r_false = NULL;


sexp* r_chr_n(const char* const * strings, r_ssize n) {
  sexp* out = KEEP(r_new_vector(r_type_character, n));

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
    sexp* out = KEEP(r_new_vector(R_TYPE, size));               \
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
    sexp* const * p_x = CONST_DEREF(x);                         \
    sexp* out = KEEP(r_new_vector(R_TYPE, size));               \
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
sexp* r_lgl_resize(sexp* x, r_ssize size) {
  RESIZE(r_type_logical, int, r_lgl_deref_const, r_lgl_deref);
}
sexp* r_int_resize(sexp* x, r_ssize size) {
  RESIZE(r_type_integer, int, r_int_deref_const, r_int_deref);
}
sexp* r_dbl_resize(sexp* x, r_ssize size) {
  RESIZE(r_type_double, double, r_dbl_deref_const, r_dbl_deref);
}
sexp* r_cpl_resize(sexp* x, r_ssize size) {
  RESIZE(r_type_complex, r_complex_t, r_cpl_deref_const, r_cpl_deref);
}
sexp* r_raw_resize(sexp* x, r_ssize size) {
  RESIZE(r_type_raw, unsigned char, r_raw_deref_const, r_raw_deref);
}
sexp* r_chr_resize(sexp* x, r_ssize size) {
  RESIZE_BARRIER(r_type_character, r_chr_deref_const, r_chr_poke);
}
sexp* r_list_resize(sexp* x, r_ssize size) {
  RESIZE_BARRIER(r_type_list, r_list_deref_const, r_list_poke);
}

#undef RESIZE
#undef RESIZE_BARRIER


sexp* r_list_compact(sexp* x) {
  r_ssize n = r_length(x);
  sexp* inc = KEEP(r_new_logical(n));

  int* v_inc = r_int_deref(inc);
  sexp* const * v_x = r_list_deref_const(x);

  r_ssize new_n = 0;
  for (r_ssize i = 0; i < n; ++i) {
    v_inc[i] = v_x[i] != r_null;
    new_n += v_inc[i];
  }

  sexp* out = KEEP(r_new_list(new_n));
  for (r_ssize i = 0, count = 0; i < n; ++i) {
    if (v_inc[i]) {
      r_list_poke(out, count, v_x[i]);
      ++count;
    }
  }

  FREE(2);
  return out;
}

sexp* r_list_of_as_ptr_ssize(sexp* xs,
                             enum r_type type,
                             struct r_pair_ptr_ssize** p_v_out) {
  if (r_typeof(xs) != r_type_list) {
    r_abort("`xs` must be a list.");
  }
  r_ssize n = r_length(xs);

  sexp* shelter = KEEP(r_new_raw(sizeof(struct r_pair_ptr_ssize) * n));
  struct r_pair_ptr_ssize* v_out = r_raw_deref(shelter);

  sexp* const * v_xs = r_list_deref_const(xs);

  for (r_ssize i = 0; i < n; ++i) {
    sexp* x = v_xs[i];
    if (r_typeof(x) != type) {
      r_abort("`xs` must be a list of vectors of type `%s`.",
              r_type_as_c_string(type));
    }

    v_out[i] = (struct r_pair_ptr_ssize) {
      .ptr = r_int_deref(x),
      .size = r_length(x)
    };
  }

  FREE(1);
  *p_v_out = v_out;
  return shelter;
}


// FIXME: Does this have a place in the library?
void r_vec_poke_n(sexp* x, r_ssize offset,
                  sexp* y, r_ssize from, r_ssize n) {

  if ((r_length(x) - offset) < n) {
    r_abort("Can't copy data to `x` because it is too small");
  }
  if ((r_length(y) - from) < n) {
    r_abort("Can't copy data from `y` because it is too small");
  }

  switch (r_typeof(x)) {
  case r_type_logical: {
    int* src_data = r_lgl_deref(y);
    int* dest_data = r_lgl_deref(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case r_type_integer: {
    int* src_data = r_int_deref(y);
    int* dest_data = r_int_deref(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case r_type_double: {
    double* src_data = r_dbl_deref(y);
    double* dest_data = r_dbl_deref(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case r_type_complex: {
    r_complex_t* src_data = r_cpl_deref(y);
    r_complex_t* dest_data = r_cpl_deref(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case RAWSXP: {
    unsigned char* src_data = RAW(y);
    unsigned char* dest_data = RAW(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case r_type_character: {
    sexp* elt;
    for (r_ssize i = 0; i != n; ++i) {
      elt = r_chr_get(y, i + from);
      r_chr_poke(x, i + offset, elt);
    }
    break;
  }
  case VECSXP: {
    sexp* elt;
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

void r_vec_poke_range(sexp* x, r_ssize offset,
                      sexp* y, r_ssize from, r_ssize to) {
  r_vec_poke_n(x, offset, y, from, to - from + 1);
}


void r_init_library_vec() {
  r_lgls_empty = r_preserve_global(r_new_logical(0));
  r_ints_empty = r_preserve_global(r_new_integer(0));
  r_dbls_empty = r_preserve_global(r_new_double(0));
  r_cpls_empty = r_preserve_global(r_new_complex(0));
  r_raws_empty = r_preserve_global(r_new_raw(0));
  r_chrs_empty = r_preserve_global(r_new_character(0));
  r_lists_empty = r_preserve_global(r_new_list(0));

  r_chrs_empty_string = r_preserve_global(r_chr(""));
  r_strs_empty = r_chr_get(r_chrs_empty_string, 0);

  r_false = r_preserve_global(r_lgl(0));
  r_true = r_preserve_global(r_lgl(1));
}
