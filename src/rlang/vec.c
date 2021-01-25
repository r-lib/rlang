#include "rlang.h"
#include <math.h>
#include <stdint.h>

sexp* r_chrs_empty = NULL;
sexp* r_strs_empty = NULL;
sexp* r_lists_empty = NULL;
sexp* r_true = NULL;
sexp* r_false = NULL;

sexp* r_chr_n(const char** strings, r_ssize n) {
  sexp* out = KEEP(r_new_vector(STRSXP, n));

  for (r_ssize i = 0; i < n; ++i) {
    r_chr_poke(out, i, r_str(strings[i]));
  }

  FREE(1);
  return out;
}


#define RESIZE(R_TYPE, C_TYPE, CONST_DEREF, DEREF)      \
  do {                                                  \
    r_ssize x_size = r_length(x);                       \
    if (x_size == size) {                               \
      return x;                                         \
    }                                                   \
                                                        \
    const C_TYPE* p_x = CONST_DEREF(x);                 \
    sexp* out = KEEP(r_new_vector(R_TYPE, size));       \
    C_TYPE* p_out = DEREF(out);                         \
                                                        \
    r_ssize cpy_size = (size > x_size) ? x_size : size; \
    memcpy(p_out, p_x, cpy_size * sizeof(C_TYPE));      \
                                                        \
    FREE(1);                                            \
    return out;                                         \
  } while (0)

#define RESIZE_BARRIER(R_TYPE, CONST_DEREF, SET)        \
  do {                                                  \
    r_ssize x_size = r_length(x);                       \
    if (x_size == size) {                               \
      return x;                                         \
    }                                                   \
                                                        \
    sexp* const * p_x = CONST_DEREF(x);                 \
    sexp* out = KEEP(r_new_vector(R_TYPE, size));       \
                                                        \
    r_ssize cpy_size = (size > x_size) ? x_size : size; \
    for (r_ssize i = 0; i < cpy_size; ++i) {            \
      SET(out, i, p_x[i]);                              \
    }                                                   \
                                                        \
    FREE(1);                                            \
    return out;                                         \
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
  case LGLSXP: {
    int* src_data = LOGICAL(y);
    int* dest_data = LOGICAL(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case INTSXP: {
    int* src_data = INTEGER(y);
    int* dest_data = INTEGER(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case REALSXP: {
    double* src_data = REAL(y);
    double* dest_data = REAL(x);
    for (r_ssize i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case CPLXSXP: {
    r_complex_t* src_data = COMPLEX(y);
    r_complex_t* dest_data = COMPLEX(x);
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
  case STRSXP: {
    sexp* elt;
    for (r_ssize i = 0; i != n; ++i) {
      elt = STRING_ELT(y, i + from);
      SET_STRING_ELT(x, i + offset, elt);
    }
    break;
  }
  case VECSXP: {
    sexp* elt;
    for (r_ssize i = 0; i != n; ++i) {
      elt = VECTOR_ELT(y, i + from);
      SET_VECTOR_ELT(x, i + offset, elt);
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
  r_lists_empty = r_preserve_global(r_new_list(0));
  r_chrs_empty = r_preserve_global(r_chr(""));
  r_strs_empty = r_chr_get(r_chrs_empty, 0);

  r_false = r_preserve_global(r_lgl(0));
  r_true = r_preserve_global(r_lgl(1));
}
