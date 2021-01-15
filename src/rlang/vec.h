#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

#include <string.h>

extern sexp* r_true;
extern sexp* r_false;

extern sexp* r_lists_empty;

extern sexp* r_chrs_empty;
extern sexp* r_strs_empty;
#define r_strings_na R_NaString


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
sexp* r_new_vector(enum r_type type, r_ssize n) {
  return Rf_allocVector(type, n);
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
sexp* r_str(const char* c_string) {
  return Rf_mkChar(c_string);
}
static inline
sexp* r_chr(const char* c_string) {
  return Rf_mkString(c_string);
}
sexp* r_chr_n(const char** strings);

static inline
SEXP r_list(SEXP x) {
  SEXP out = r_new_vector(r_type_list, 1);
  r_list_poke(out, 0, x);
  return out;
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
bool r_is_bool(SEXP x) {
  return
    r_typeof(x) == r_type_logical &&
    r_length(x) == 1 &&
    r_lgl_get(x, 0) != NA_LOGICAL;
}
static inline
bool r_is_int(SEXP x) {
  return
    r_typeof(x) == r_type_integer &&
    r_length(x) == 1 &&
    r_int_get(x, 0) != NA_INTEGER;
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
    r_typeof(x) == r_type_character &&
    r_length(x) == 1 &&
    r_chr_get(x, 0) != R_NaString;
}

sexp* r_lgl_resize(sexp* x, r_ssize size);
sexp* r_int_resize(sexp* x, r_ssize size);
sexp* r_dbl_resize(sexp* x, r_ssize size);
sexp* r_cpl_resize(sexp* x, r_ssize size);
sexp* r_raw_resize(sexp* x, r_ssize size);
sexp* r_chr_resize(sexp* x, r_ssize size);
sexp* r_list_resize(sexp* x, r_ssize size);

static inline
sexp* r_copy_in_raw(const void* src, size_t size) {
  sexp* out = r_new_vector(r_type_raw, size);
  memcpy(r_raw_deref(out), src, size);
  return out;
}


#endif
