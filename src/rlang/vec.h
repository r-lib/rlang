#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

#include <string.h>


extern sexp* r_shared_empty_list;

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
sexp* r_list_get(sexp* list, r_ssize i) {
  return VECTOR_ELT(list, i);
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
void r_list_poke(sexp* list, r_ssize i, sexp* elt) {
  SET_VECTOR_ELT(list, i, elt);
}


static inline
bool r_is_int(SEXP x) {
  return r_typeof(x) == r_type_integer &&
    r_length(x) == 1 &&
    r_int_get(x, 0) != NA_INTEGER;
}

static inline
sexp* r_int(int x) {
  return Rf_ScalarInteger(x);
}

static inline
sexp* r_new_vector(enum r_type type, r_ssize n) {
  return Rf_allocVector(type, n);
}


static inline
sexp* r_copy_in_raw(void* src, size_t size) {
  sexp* out = r_new_vector(r_type_raw, size);
  memcpy(r_raw_deref(out), src, size);
  return out;
}


#endif
