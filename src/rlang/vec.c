#include "rlang.h"
#include <math.h>
#include <stdint.h>

sexp* r_shared_empty_list = NULL;
sexp* r_chrs_empty = NULL;
sexp* r_strs_empty = NULL;


static
r_ssize ptrs_array_length(void** ptrs) {
  r_ssize n = 0;

  while (*ptrs) {
    ++ptrs;
    ++n;
  }

  return n;
}

sexp* r_chr_n(const char** strings) {
  r_ssize n = ptrs_array_length((void**) strings);
  sexp* out = KEEP(r_new_vector(STRSXP, n));

  for (r_ssize i = 0; i < n; ++i) {
    r_chr_poke(out, i, r_str(strings[i]));
  }

  FREE(1);
  return out;
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
  r_shared_empty_list = r_new_vector(r_type_list, 0);
  r_mark_shared(r_shared_empty_list);
  r_mark_precious(r_shared_empty_list);

  r_chrs_empty = r_chr("");
  r_mark_shared(r_chrs_empty);
  r_mark_precious(r_chrs_empty);

  r_strs_empty = r_chr_get(r_chrs_empty, 0);
}
