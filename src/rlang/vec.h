#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H


r_size_t r_vec_length(SEXP x);

bool r_is_list(SEXP x);
bool r_is_vector(SEXP x);
bool r_is_scalar_atomic(SEXP x);
bool r_is_atomic(SEXP x);
bool r_is_integerish(SEXP x);

static inline SEXP r_scalar_lgl(bool x) {
  return Rf_ScalarLogical(x);
}
static inline SEXP r_scalar_int(int x) {
  return Rf_ScalarInteger(x);
}

static inline int r_c_int(SEXP x) {
  return INTEGER(x)[0];
}

static inline SEXP r_new_vector(enum r_type type, r_size_t n) {
  return Rf_allocVector(type, n);
}
static inline sexp* r_vec_coerce(sexp* x, enum r_type to) {
  return Rf_coerceVector(x, to);
}

void r_vec_poke_n(SEXP x, r_size_t offset,
                  SEXP y, r_size_t from, r_size_t n);
void r_vec_poke_range(SEXP x, r_size_t offset,
                      SEXP y, r_size_t from, r_size_t to);

void r_vec_poke_coerce_n(SEXP x, r_size_t offset,
                         SEXP y, r_size_t from, r_size_t n);
void r_vec_poke_coerce_range(SEXP x, r_size_t offset,
                             SEXP y, r_size_t from, r_size_t to);


#endif
