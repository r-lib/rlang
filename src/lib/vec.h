#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H


r_ssize r_vec_length(sexp* x);


static inline int* r_lgl_deref(sexp* x) {
  return LOGICAL(x);
}
static inline int* r_int_deref(sexp* x) {
  return INTEGER(x);
}
static inline double* r_dbl_deref(sexp* x) {
  return REAL(x);
}
static inline r_complex_t* r_cpl_deref(sexp* x) {
  return COMPLEX(x);
}
static inline r_byte_t* r_raw_deref(sexp* x) {
  return RAW(x);
}
static inline sexp** r_chr_deref(sexp* x) {
  return STRING_PTR(x);
}
static inline sexp** r_list_deref(sexp* x) {
  return VECTOR_PTR(x);
}

static inline void r_vec_get_check(sexp*x, r_ssize i, const char* fn) {
  if ((r_length(x) - 1) < i) {
    r_abort("Internal error in `%s()`: Vector is too small", fn);
  }
}

static inline int r_lgl_get(sexp* x, r_ssize i) {
  r_vec_get_check(x, i, "r_lgl_get");
  return LOGICAL(x)[i];
}
static inline int r_int_get(sexp* x, r_ssize i) {
  r_vec_get_check(x, i, "r_int_get");
  return INTEGER(x)[i];
}
static inline double r_dbl_get(sexp* x, r_ssize i) {
  r_vec_get_check(x, i, "r_dbl_get");
  return REAL(x)[i];
}
static inline r_complex_t r_cpl_get(sexp* x, r_ssize i) {
  r_vec_get_check(x, i, "r_cpl_get");
  return COMPLEX(x)[i];
}
static inline r_byte_t r_raw_get(sexp* x, r_ssize i) {
  r_vec_get_check(x, i, "r_raw_get");
  return RAW(x)[i];
}

static inline void r_lgl_poke(sexp* x, r_ssize i, int y) {
  r_vec_get_check(x, i, "r_lgl_poke");
  LOGICAL(x)[i] = y;
}
static inline void r_int_poke(sexp* x, r_ssize i, int y) {
  r_vec_get_check(x, i, "r_int_poke");
  INTEGER(x)[i] = y;
}
static inline void r_dbl_poke(sexp* x, r_ssize i, double y) {
  r_vec_get_check(x, i, "r_dbl_poke");
  REAL(x)[i] = y;
}
static inline void r_cpl_poke(sexp* x, r_ssize i, r_complex_t y) {
  r_vec_get_check(x, i, "r_cpl_poke");
  COMPLEX(x)[i] = y;
}
static inline void r_raw_poke(sexp* x, r_ssize i, r_byte_t y) {
  r_vec_get_check(x, i, "r_raw_poke");
  RAW(x)[i] = y;
}

sexp* r_vec_get(sexp* vec, r_ssize i);


bool r_is_vector(sexp* x, r_ssize n);
bool r_is_scalar_atomic(sexp* x);
bool r_is_atomic(sexp* x, r_ssize n);

bool r_is_finite(sexp* x);
bool r_is_logical(sexp* x, r_ssize n);
bool r_is_integerish(sexp* x, r_ssize n, int finite);
bool r_is_integer(sexp* x, r_ssize n, int finite);
bool r_is_double(sexp* x, r_ssize n, int finite);
bool r_is_character(sexp* x, r_ssize n);
bool r_is_raw(sexp* x, r_ssize n);


static inline bool r_is_scalar_character(sexp* x) {
  return r_typeof(x) == r_type_character && r_length(x) == 1;
}
static inline bool r_is_scalar_logical(sexp* x) {
  return r_typeof(x) == r_type_logical && r_length(x) == 1;
}

static inline sexp* r_int(int x) {
  return Rf_ScalarInteger(x);
}

static inline int r_c_int(sexp* x) {
  return INTEGER(x)[0];
}

static inline sexp* r_new_vector(enum r_type type, r_ssize n) {
  return Rf_allocVector(type, n);
}
static inline sexp* r_vec_coerce(sexp* x, enum r_type to) {
  return Rf_coerceVector(x, to);
}

void r_vec_poke_n(sexp* x, r_ssize offset,
                  sexp* y, r_ssize from, r_ssize n);
void r_vec_poke_range(sexp* x, r_ssize offset,
                      sexp* y, r_ssize from, r_ssize to);

void r_vec_poke_coerce_n(sexp* x, r_ssize offset,
                         sexp* y, r_ssize from, r_ssize n);
void r_vec_poke_coerce_range(sexp* x, r_ssize offset,
                             sexp* y, r_ssize from, r_ssize to);

static inline bool r_vec_find_first_duplicate(sexp* x, sexp* except, r_ssize* index) {
  r_ssize idx;
  if (except) {
    idx = Rf_any_duplicated3(x, except, false);
  } else {
    idx = Rf_any_duplicated(x, false);
  }

  if (idx) {
    if (index) {
      *index = idx - 1;
    }
    return true;
  } else {
    return false;
  }
}

static inline sexp* r_vec_are_duplicated(sexp* x) {
  return Rf_duplicated(x, false);
}

bool r_vec_find_first_identical_any(sexp* x, sexp* y, r_ssize* index);


#endif
