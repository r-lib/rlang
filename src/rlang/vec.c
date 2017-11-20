#include "rlang.h"


bool r_is_atomic(SEXP x) {
  switch(r_kind(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return true;
  default:
    return false;
  }
}
bool r_is_scalar_atomic(SEXP x) {
  return r_length(x) == 1 && r_is_atomic(x);
}

bool r_is_integerish(SEXP x) {
  static SEXP predicate = NULL;
  if (!predicate) {
    predicate = rlang_ns_get("is_integerish");
  }
  SEXP call = KEEP(r_build_call1(predicate, x));
  SEXP out = r_eval(call, r_empty_env);
  FREE(1);
  return out;
}

bool r_is_list(SEXP x) {
  return r_kind(x) == VECSXP;
}
bool r_is_vector(SEXP x) {
  switch(r_kind(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return true;
  default:
    return false;
  }
}

r_size_t r_vec_length(SEXP x) {
  switch(r_kind(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
  case NILSXP:
    return r_length(x);
  default:
    r_abort("Internal error: expected a vector");
  }
}


// Copy --------------------------------------------------------------

void r_vec_poke_n(SEXP x, r_size_t offset,
                  SEXP y, r_size_t from, r_size_t n) {

  if ((r_length(x) - offset) < n) {
    r_abort("Can't copy data to `x` because it is too small");
  }
  if ((r_length(y) - from) < n) {
    r_abort("Can't copy data from `y` because it is too small");
  }

  switch (r_kind(x)) {
  case LGLSXP: {
    int* src_data = LOGICAL(y);
    int* dest_data = LOGICAL(x);
    for (r_size_t i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case INTSXP: {
    int* src_data = INTEGER(y);
    int* dest_data = INTEGER(x);
    for (r_size_t i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case REALSXP: {
    double* src_data = REAL(y);
    double* dest_data = REAL(x);
    for (r_size_t i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case CPLXSXP: {
    r_complex_t* src_data = COMPLEX(y);
    r_complex_t* dest_data = COMPLEX(x);
    for (r_size_t i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case RAWSXP: {
    r_byte_t* src_data = RAW(y);
    r_byte_t* dest_data = RAW(x);
    for (r_size_t i = 0; i != n; ++i)
      dest_data[i + offset] = src_data[i + from];
    break;
  }
  case STRSXP: {
    SEXP elt;
    for (r_size_t i = 0; i != n; ++i) {
      elt = STRING_ELT(y, i + from);
      SET_STRING_ELT(x, i + offset, elt);
    }
    break;
  }
  case VECSXP: {
    SEXP elt;
    for (r_size_t i = 0; i != n; ++i) {
      elt = VECTOR_ELT(y, i + from);
      SET_VECTOR_ELT(x, i + offset, elt);
    }
    break;
  }
  default:
    r_abort("Copy requires vectors");
  }
}

void r_vec_poke_range(SEXP x, r_size_t offset,
                      SEXP y, r_size_t from, r_size_t to) {
  r_vec_poke_n(x, offset, y, from, to - from + 1);
}


// Coercion ----------------------------------------------------------

SEXP rlang_vec_coercer(SEXP dest) {
  switch(r_kind(dest)) {
  case LGLSXP: return rlang_ns_get("as_logical");
  case INTSXP: return rlang_ns_get("as_integer");
  case REALSXP: return rlang_ns_get("as_double");
  case CPLXSXP: return rlang_ns_get("as_complex");
  case STRSXP: return rlang_ns_get("as_character");
  case RAWSXP: return rlang_ns_get("as_bytes");
  default: r_abort("No coercion implemented for `%s`", Rf_type2str(r_kind(dest)));
  }
}

void r_vec_poke_coerce_n(SEXP x, r_size_t offset,
                         SEXP y, r_size_t from, r_size_t n) {
  if (r_kind(y) == r_kind(x)) {
    r_vec_poke_n(x, offset, y, from, n);
    return ;
  }
  if (r_is_object(y)) {
    r_abort("Can't splice S3 objects");
  }

  // FIXME: This callbacks to rlang R coercers with an extra copy.
  SEXP coercer = rlang_vec_coercer(x);
  SEXP call = KEEP(Rf_lang2(coercer, y));
  SEXP coerced = KEEP(r_eval(call, R_BaseEnv));

  r_vec_poke_n(x, offset, coerced, from, n);
  FREE(2);
}

void r_vec_poke_coerce_range(SEXP x, r_size_t offset,
                             SEXP y, r_size_t from, r_size_t to) {
  r_vec_poke_coerce_n(x, offset, y, from, to - from + 1);
}
