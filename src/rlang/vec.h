#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

#include <string.h>


static inline
int* r_lgl_deref(sexp* x) {
  return LOGICAL(x);
}
static inline
int* r_int_deref(sexp* x) {
  return INTEGER(x);
}
static inline
double* r_dbl_deref(sexp* x) {
  return REAL(x);
}
static inline
r_complex_t* r_cpl_deref(sexp* x) {
  return COMPLEX(x);
}
static inline
void* r_raw_deref(sexp* x) {
  return RAW(x);
}

static inline
const int* r_int_deref_const(sexp* x) {
  return (const int*) INTEGER(x);
}
static inline
const int* r_lgl_deref_const(sexp* x) {
  return (const int*) LOGICAL(x);
}
static inline
const double* r_dbl_deref_const(sexp* x) {
  return (const double*) REAL(x);
}
static inline
const r_complex_t* r_cpl_deref_const(sexp* x) {
  return (const r_complex_t*) COMPLEX(x);
}
static inline
const void* r_raw_deref_const(sexp* x) {
  return (const void*) RAW(x);
}
static inline
sexp* const * r_chr_deref_const(sexp* x) {
  return (sexp* const *) STRING_PTR(x);
}
static inline
sexp* const * r_list_deref_const(sexp* x) {
#if (R_VERSION < R_Version(3, 5, 0))
  return ((sexp* const *) STRING_PTR(x));
#else
  return ((sexp* const *) DATAPTR_RO(x));
#endif
}

static inline
void* r_vec_deref0(enum r_type type, sexp* x) {
  switch (type) {
  case R_TYPE_logical: return r_lgl_deref(x);
  case R_TYPE_integer: return r_int_deref(x);
  case R_TYPE_double: return r_dbl_deref(x);
  case R_TYPE_complex: return r_cpl_deref(x);
  case R_TYPE_raw: return r_raw_deref(x);
  default: r_stop_unimplemented_type("r_vec_deref", type);
  }
}
static inline
void* r_vec_deref(sexp* x) {
  return r_vec_deref0(r_typeof(x), x);
}

static inline
const void* r_vec_deref_const0(enum r_type type, sexp* x) {
  switch (type) {
  case R_TYPE_logical: return r_lgl_deref_const(x);
  case R_TYPE_integer: return r_int_deref_const(x);
  case R_TYPE_double: return r_dbl_deref_const(x);
  case R_TYPE_complex: return r_cpl_deref_const(x);
  case R_TYPE_raw: return r_raw_deref_const(x);
  case R_TYPE_character: return r_chr_deref_const(x);
  case R_TYPE_list: return r_list_deref_const(x);
  default: r_stop_unimplemented_type("r_vec_deref_const", type);
  }
}
static inline
const void* r_vec_deref_const(sexp* x) {
  return r_vec_deref_const0(r_typeof(x), x);
}

static inline
int r_vec_elt_sizeof0(enum r_type type) {
  switch (type) {
  case R_TYPE_logical: return sizeof(int);
  case R_TYPE_integer: return sizeof(int);
  case R_TYPE_double: return sizeof(double);
  case R_TYPE_complex: return sizeof(r_complex_t);
  case R_TYPE_raw: return sizeof(char);
  case R_TYPE_character: return sizeof(sexp*);
  case R_TYPE_list: return sizeof(sexp*);
  default: r_stop_unimplemented_type("r_vec_elt_sizeof", type);
  }
}
static inline
int r_vec_elt_sizeof(sexp* x) {
  return r_vec_elt_sizeof0(r_typeof(x));
}

static inline
int r_lgl_get(sexp* x, r_ssize i) {
  return LOGICAL(x)[i];
}
static inline
int r_int_get(sexp* x, r_ssize i) {
  return INTEGER(x)[i];
}
static inline
double r_dbl_get(sexp* x, r_ssize i) {
  return REAL(x)[i];
}
static inline
r_complex_t r_cpl_get(sexp* x, r_ssize i) {
  return COMPLEX(x)[i];
}
static inline
sexp* r_chr_get(sexp* x, r_ssize i) {
  return STRING_ELT(x, i);
}
static inline
const char* r_chr_get_c_string(sexp* x, r_ssize i) {
  return CHAR(r_chr_get(x, i));
}
static inline
sexp* r_list_get(sexp* x, r_ssize i) {
  return VECTOR_ELT(x, i);
}

static inline
void r_lgl_poke(sexp* x, r_ssize i, int y) {
  LOGICAL(x)[i] = y;
}
static inline
void r_int_poke(sexp* x, r_ssize i, int y) {
  INTEGER(x)[i] = y;
}
static inline
void r_dbl_poke(sexp* x, r_ssize i, double y) {
  REAL(x)[i] = y;
}
static inline
void r_cpl_poke(sexp* x, r_ssize i, r_complex_t y) {
  COMPLEX(x)[i] = y;
}
static inline
void r_chr_poke(sexp* x, r_ssize i, sexp* y) {
  SET_STRING_ELT(x, i, y);
}
static inline
void r_list_poke(sexp* x, r_ssize i, sexp* y) {
  SET_VECTOR_ELT(x, i, y);
}


static inline
sexp* r_alloc_vector(enum r_type type, r_ssize n) {
  return Rf_allocVector(type, n);
}
static inline
sexp* r_alloc_logical(r_ssize n) {
  return Rf_allocVector(R_TYPE_logical, n);
}
static inline
sexp* r_alloc_integer(r_ssize n) {
  return Rf_allocVector(R_TYPE_integer, n);
}
static inline
sexp* r_alloc_double(r_ssize n) {
  return Rf_allocVector(R_TYPE_double, n);
}
static inline
sexp* r_alloc_complex(r_ssize n) {
  return Rf_allocVector(R_TYPE_complex, n);
}
static inline
sexp* r_alloc_raw(r_ssize n) {
  return Rf_allocVector(R_TYPE_raw, n);
}
static inline
sexp* r_alloc_character(r_ssize n) {
  return Rf_allocVector(R_TYPE_character, n);
}
static inline
sexp* r_alloc_list(r_ssize n) {
  return Rf_allocVector(R_TYPE_list, n);
}

static inline
sexp* r_alloc_raw0(r_ssize n) {
  sexp* out = r_alloc_raw(n);

  unsigned char* p_out = (unsigned char*) r_raw_deref(out);
  memset(p_out, 0, n);

  return out;
}

static inline
sexp* r_lgl(bool x) {
  return Rf_ScalarLogical(x);
}
static inline
sexp* r_int(int x) {
  return Rf_ScalarInteger(x);
}
static inline
sexp* r_dbl(double x) {
  return Rf_ScalarReal(x);
}
static inline
sexp* r_str(const char* c_string) {
  return Rf_mkChar(c_string);
}
static inline
sexp* r_chr(const char* c_string) {
  return Rf_mkString(c_string);
}
static inline
sexp* r_list(sexp* x) {
  sexp* out = r_alloc_list(1);
  r_list_poke(out, 0, x);
  return out;
}

sexp* r_chr_n(const char* const * strings, r_ssize n);

static inline
sexp* r_len(r_ssize x) {
  if (x > INT_MAX) {
    return r_dbl(x);
  } else {
    return r_int(x);
  }
}


// FIXME: Redundant with `r_lgl()`
static inline
sexp* r_shared_lgl(bool x) {
  if (x) {
    return r_true;
  } else {
    return r_false;
  }
}

static inline
bool r_is_bool(sexp* x) {
  return
    r_typeof(x) == R_TYPE_logical &&
    r_length(x) == 1 &&
    r_lgl_get(x, 0) != r_globals.na_lgl;
}
static inline
bool r_is_int(sexp* x) {
  return
    r_typeof(x) == R_TYPE_integer &&
    r_length(x) == 1 &&
    r_int_get(x, 0) != r_globals.na_int;
}
static inline
bool r_is_true(sexp* x) {
  if (r_is_bool(x)) {
    return r_lgl_get(x, 0);
  } else {
    return false;
  }
}
static inline
bool r_is_string(sexp* x) {
  return
    r_typeof(x) == R_TYPE_character &&
    r_length(x) == 1 &&
    r_chr_get(x, 0) != R_NaString;
}

static inline
bool r_as_bool(sexp* x) {
  if (!r_is_bool(x)) {
    r_abort("`x` must be a logical value");
  }
  return r_lgl_get(x, 0);
}
static inline
int r_as_int(sexp* x) {
  if (!r_is_int(x)) {
    r_abort("`x` must be an integer value");
  }
  return r_int_get(x, 0);
}

sexp* r_lgl_resize(sexp* x, r_ssize size);
sexp* r_int_resize(sexp* x, r_ssize size);
sexp* r_dbl_resize(sexp* x, r_ssize size);
sexp* r_cpl_resize(sexp* x, r_ssize size);
sexp* r_raw_resize(sexp* x, r_ssize size);
sexp* r_chr_resize(sexp* x, r_ssize size);
sexp* r_list_resize(sexp* x, r_ssize size);

static inline
sexp* r_vec_resize0(enum r_type type, sexp* x, r_ssize size) {
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
sexp* r_vec_resize(sexp* x, r_ssize size) {
  return r_vec_resize0(r_typeof(x), x, size);
}

static inline
sexp* r_vec_n(enum r_type type, void* v_src, r_ssize n) {
  switch (type) {
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_raw: {
    sexp* out = r_alloc_vector(type, n);
    memcpy(r_vec_deref(out), v_src, n * r_vec_elt_sizeof0(type));
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
sexp* r_lgl_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_logical, v_src, n);
}
static inline
sexp* r_int_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_integer, v_src, n);
}
static inline
sexp* r_dbl_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_double, v_src, n);
}
static inline
sexp* r_cpl_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_complex, v_src, n);
}
static inline
sexp* r_raw_n(int* v_src, r_ssize n) {
  return r_vec_n(R_TYPE_raw, v_src, n);
}


static inline
sexp* r_copy_in_raw(const void* src, size_t size) {
  sexp* out = r_alloc_raw(size);
  memcpy(r_raw_deref(out), src, size);
  return out;
}

static inline
void r_int_fill_iota0(int* p_x, int start, r_ssize n) {
  for (r_ssize i = 0; i < n; ++i) {
    p_x[i] = start++;
  }
}
static inline
void r_int_fill_iota(sexp* x) {
  r_int_fill_iota0(r_int_deref(x), 0, r_length(x));
}


sexp* r_list_compact(sexp* x);

sexp* r_list_of_as_ptr_ssize(sexp* xs,
                             enum r_type type,
                             struct r_pair_ptr_ssize** p_v_out);


// From cpp/vec.cpp

int* r_int_unique0(int* v_data, r_ssize size);

static inline
int* r_int_unique(sexp* x) {
  return r_int_unique0(r_int_deref(x), r_length(x));
}


#endif
