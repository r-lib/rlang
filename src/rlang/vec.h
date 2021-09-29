#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

#include <string.h>


static inline
int* r_lgl_begin(r_obj* x) {
  return LOGICAL(x);
}
static inline
int* r_int_begin(r_obj* x) {
  return INTEGER(x);
}
static inline
double* r_dbl_begin(r_obj* x) {
  return REAL(x);
}
static inline
r_complex_t* r_cpl_begin(r_obj* x) {
  return COMPLEX(x);
}
static inline
void* r_raw_begin(r_obj* x) {
  return RAW(x);
}

static inline
const int* r_int_cbegin(r_obj* x) {
  return (const int*) INTEGER(x);
}
static inline
const int* r_lgl_cbegin(r_obj* x) {
  return (const int*) LOGICAL(x);
}
static inline
const double* r_dbl_cbegin(r_obj* x) {
  return (const double*) REAL(x);
}
static inline
const r_complex_t* r_cpl_cbegin(r_obj* x) {
  return (const r_complex_t*) COMPLEX(x);
}
static inline
const void* r_raw_cbegin(r_obj* x) {
  return (const void*) RAW(x);
}
static inline
r_obj* const * r_chr_cbegin(r_obj* x) {
  return (r_obj* const *) STRING_PTR(x);
}
static inline
r_obj* const * r_list_cbegin(r_obj* x) {
#if (R_VERSION < R_Version(3, 5, 0))
  return ((r_obj* const *) STRING_PTR(x));
#else
  return ((r_obj* const *) DATAPTR_RO(x));
#endif
}

static inline
void* r_vec_begin0(enum r_type type, r_obj* x) {
  switch (type) {
  case R_TYPE_logical: return r_lgl_begin(x);
  case R_TYPE_integer: return r_int_begin(x);
  case R_TYPE_double: return r_dbl_begin(x);
  case R_TYPE_complex: return r_cpl_begin(x);
  case R_TYPE_raw: return r_raw_begin(x);
  default: r_stop_unimplemented_type("r_vec_begin", type);
  }
}
static inline
void* r_vec_begin(r_obj* x) {
  return r_vec_begin0(r_typeof(x), x);
}

static inline
const void* r_vec_cbegin0(enum r_type type, r_obj* x) {
  switch (type) {
  case R_TYPE_logical: return r_lgl_cbegin(x);
  case R_TYPE_integer: return r_int_cbegin(x);
  case R_TYPE_double: return r_dbl_cbegin(x);
  case R_TYPE_complex: return r_cpl_cbegin(x);
  case R_TYPE_raw: return r_raw_cbegin(x);
  case R_TYPE_character: return r_chr_cbegin(x);
  case R_TYPE_list: return r_list_cbegin(x);
  default: r_stop_unimplemented_type("r_vec_cbegin", type);
  }
}
static inline
const void* r_vec_cbegin(r_obj* x) {
  return r_vec_cbegin0(r_typeof(x), x);
}

static inline
int r_vec_elt_sizeof0(enum r_type type) {
  switch (type) {
  case R_TYPE_logical: return sizeof(int);
  case R_TYPE_integer: return sizeof(int);
  case R_TYPE_double: return sizeof(double);
  case R_TYPE_complex: return sizeof(r_complex_t);
  case R_TYPE_raw: return sizeof(char);
  case R_TYPE_character: return sizeof(r_obj*);
  case R_TYPE_list: return sizeof(r_obj*);
  default: r_stop_unimplemented_type("r_vec_elt_sizeof", type);
  }
}
static inline
int r_vec_elt_sizeof(r_obj* x) {
  return r_vec_elt_sizeof0(r_typeof(x));
}

static inline
int r_lgl_get(r_obj* x, r_ssize i) {
  return LOGICAL(x)[i];
}
static inline
int r_int_get(r_obj* x, r_ssize i) {
  return INTEGER(x)[i];
}
static inline
double r_dbl_get(r_obj* x, r_ssize i) {
  return REAL(x)[i];
}
static inline
r_complex_t r_cpl_get(r_obj* x, r_ssize i) {
  return COMPLEX(x)[i];
}
static inline
r_obj* r_chr_get(r_obj* x, r_ssize i) {
  return STRING_ELT(x, i);
}
static inline
const char* r_chr_get_c_string(r_obj* x, r_ssize i) {
  return CHAR(r_chr_get(x, i));
}
static inline
r_obj* r_list_get(r_obj* x, r_ssize i) {
  return VECTOR_ELT(x, i);
}

static inline
void r_lgl_poke(r_obj* x, r_ssize i, int y) {
  LOGICAL(x)[i] = y;
}
static inline
void r_int_poke(r_obj* x, r_ssize i, int y) {
  INTEGER(x)[i] = y;
}
static inline
void r_dbl_poke(r_obj* x, r_ssize i, double y) {
  REAL(x)[i] = y;
}
static inline
void r_cpl_poke(r_obj* x, r_ssize i, r_complex_t y) {
  COMPLEX(x)[i] = y;
}
static inline
void r_chr_poke(r_obj* x, r_ssize i, r_obj* y) {
  SET_STRING_ELT(x, i, y);
}
static inline
void r_list_poke(r_obj* x, r_ssize i, r_obj* y) {
  SET_VECTOR_ELT(x, i, y);
}

#define r_chr_poke(X, I, Y) SET_STRING_ELT(X, I, Y)
#define r_list_poke(X, I, Y) SET_VECTOR_ELT(X, I, Y)

static inline
r_obj* r_alloc_vector(enum r_type type, r_ssize n) {
  return Rf_allocVector(type, n);
}
static inline
r_obj* r_alloc_logical(r_ssize n) {
  return Rf_allocVector(R_TYPE_logical, n);
}
static inline
r_obj* r_alloc_integer(r_ssize n) {
  return Rf_allocVector(R_TYPE_integer, n);
}
static inline
r_obj* r_alloc_double(r_ssize n) {
  return Rf_allocVector(R_TYPE_double, n);
}
static inline
r_obj* r_alloc_complex(r_ssize n) {
  return Rf_allocVector(R_TYPE_complex, n);
}
static inline
r_obj* r_alloc_raw(r_ssize n) {
  return Rf_allocVector(R_TYPE_raw, n);
}
static inline
r_obj* r_alloc_character(r_ssize n) {
  return Rf_allocVector(R_TYPE_character, n);
}
static inline
r_obj* r_alloc_list(r_ssize n) {
  return Rf_allocVector(R_TYPE_list, n);
}

static inline
r_obj* r_alloc_raw0(r_ssize n) {
  r_obj* out = r_alloc_raw(n);

  unsigned char* p_out = (unsigned char*) r_raw_begin(out);
  memset(p_out, 0, n);

  return out;
}

static inline
r_obj* r_lgl(bool x) {
  return Rf_ScalarLogical(x);
}
static inline
r_obj* r_int(int x) {
  return Rf_ScalarInteger(x);
}
static inline
r_obj* r_dbl(double x) {
  return Rf_ScalarReal(x);
}
static inline
r_obj* r_cpl(r_complex_t x) {
  return Rf_ScalarComplex(x);
}
static inline
r_obj* r_str(const char* c_string) {
  return Rf_mkCharCE(c_string, CE_UTF8);
}
static inline
r_obj* r_chr(const char* c_string) {
  r_obj* out = KEEP(r_alloc_character(1));
  r_chr_poke(out, 0, r_str(c_string));
  FREE(1);
  return out;
}
static inline
r_obj* r_list(r_obj* x) {
  r_obj* out = r_alloc_list(1);
  r_list_poke(out, 0, x);
  return out;
}

r_obj* r_chr_n(const char* const * strings, r_ssize n);

static inline
r_obj* r_len(r_ssize x) {
  if (x > INT_MAX) {
    return r_dbl(x);
  } else {
    return r_int(x);
  }
}


// FIXME: Redundant with `r_lgl()`
static inline
r_obj* r_shared_lgl(bool x) {
  if (x) {
    return r_true;
  } else {
    return r_false;
  }
}

static inline
bool r_is_bool(r_obj* x) {
  return
    r_typeof(x) == R_TYPE_logical &&
    r_length(x) == 1 &&
    r_lgl_get(x, 0) != r_globals.na_lgl;
}
static inline
bool r_is_int(r_obj* x) {
  return
    r_typeof(x) == R_TYPE_integer &&
    r_length(x) == 1 &&
    r_int_get(x, 0) != r_globals.na_int;
}
static inline
bool r_is_true(r_obj* x) {
  return r_is_bool(x) && r_lgl_get(x, 0);
}
static inline
bool r_is_false(r_obj* x) {
  return r_is_bool(x) && !r_lgl_get(x, 0);
}
static inline
bool r_is_string(r_obj* x) {
  return
    r_typeof(x) == R_TYPE_character &&
    r_length(x) == 1 &&
    r_chr_get(x, 0) != R_NaString;
}

static inline
bool r_arg_as_bool(r_obj* x, const char* arg) {
  if (!r_is_bool(x)) {
    r_abort("`%s` must be a logical value.", arg);
  }
  return r_lgl_get(x, 0);
}
static inline
bool r_as_bool(r_obj* x) {
  return r_arg_as_bool(x, "x");
}
static inline
int r_arg_as_int(r_obj* x, const char* arg) {
  if (!r_is_int(x)) {
    r_abort("`%s` must be an integer value.", arg);
  }
  return r_int_get(x, 0);
}
static inline
int r_as_int(r_obj* x) {
  return r_arg_as_int(x, "x");
}

r_obj* r_lgl_resize(r_obj* x, r_ssize size);
r_obj* r_int_resize(r_obj* x, r_ssize size);
r_obj* r_dbl_resize(r_obj* x, r_ssize size);
r_obj* r_cpl_resize(r_obj* x, r_ssize size);
r_obj* r_raw_resize(r_obj* x, r_ssize size);
r_obj* r_chr_resize(r_obj* x, r_ssize size);
r_obj* r_list_resize(r_obj* x, r_ssize size);

static inline
r_obj* r_vec_resize0(enum r_type type, r_obj* x, r_ssize size) {
  switch (type) {
  case R_TYPE_logical: return r_lgl_resize(x, size);
  case R_TYPE_integer: return r_int_resize(x, size);
  case R_TYPE_double: return r_dbl_resize(x, size);
  case R_TYPE_complex: return r_cpl_resize(x, size);
  case R_TYPE_raw: return r_raw_resize(x, size);
  case R_TYPE_character: return r_chr_resize(x, size);
  case R_TYPE_list: return r_list_resize(x, size);
  default: r_stop_unimplemented_type("r_vec_resize", type);
  }
}
static inline
r_obj* r_vec_resize(r_obj* x, r_ssize size) {
  return r_vec_resize0(r_typeof(x), x, size);
}

static inline
r_obj* r_vec_n(enum r_type type, void* v_src, r_ssize n) {
  switch (type) {
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_raw: {
    r_obj* out = r_alloc_vector(type, n);
    memcpy(r_vec_begin(out), v_src, n * r_vec_elt_sizeof0(type));
    return out;
  }
  case R_TYPE_character:
  case R_TYPE_list:
    r_abort("TODO: barrier types in `r_vec_n()`");
  default:
    r_stop_unimplemented_type("r_vec_n", type);
  }
}

static inline
r_obj* r_lgl_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_logical, v_src, n);
}
static inline
r_obj* r_int_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_integer, v_src, n);
}
static inline
r_obj* r_dbl_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_double, v_src, n);
}
static inline
r_obj* r_cpl_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_complex, v_src, n);
}
static inline
r_obj* r_raw_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_raw, v_src, n);
}


static inline
r_obj* r_copy_in_raw(const void* src, size_t size) {
  r_obj* out = r_alloc_raw(size);
  memcpy(r_raw_begin(out), src, size);
  return out;
}

static inline
void r_int_fill_iota0(int* p_x, int start, r_ssize n) {
  for (r_ssize i = 0; i < n; ++i) {
    p_x[i] = start++;
  }
}
static inline
void r_int_fill_iota(r_obj* x) {
  r_int_fill_iota0(r_int_begin(x), 0, r_length(x));
}


r_obj* r_list_compact(r_obj* x);

r_obj* r_list_of_as_ptr_ssize(r_obj* xs,
                              enum r_type type,
                              struct r_pair_ptr_ssize** p_v_out);


// From cpp/vec.cpp

int* r_int_unique0(int* v_data, r_ssize size);

static inline
int* r_int_unique(r_obj* x) {
  return r_int_unique0(r_int_begin(x), r_length(x));
}


#endif
