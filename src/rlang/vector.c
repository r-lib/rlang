#include "rlang.h"


// In particular, this returns 1 for environments
r_size_t vec_length(SEXP x) {
  switch (r_kind(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return r_length(x);
  case NILSXP:
    return 0;
  default:
    return 1;
  }
}

SEXP r_scalar_lgl(bool x) {
  return Rf_ScalarLogical(x);
}


// Copy --------------------------------------------------------------

void r_vec_poke_from(SEXP x, r_size_t offset,
                     SEXP y, r_size_t from, r_size_t to) {
  if (to == -1) {
    to = r_length(y);
  }
  r_size_t n = to - from;

  if ((r_length(x) - offset) < n) {
    r_abort("Can't copy data to `x` because it is too small");
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


// Coercion ----------------------------------------------------------

SEXP rlang_vec_coercer(SEXP dest) {
  switch(r_kind(dest)) {
  case LGLSXP: return rlang_obj("as_logical");
  case INTSXP: return rlang_obj("as_integer");
  case REALSXP: return rlang_obj("as_double");
  case CPLXSXP: return rlang_obj("as_complex");
  case STRSXP: return rlang_obj("as_character");
  case RAWSXP: return rlang_obj("as_bytes");
  default: r_abort("No coercion implemented for `%s`", Rf_type2str(r_kind(dest)));
  }
}

// Might allocate, caller must protect result
void r_vec_poke_coerce_from(SEXP x, r_size_t offset,
                            SEXP y, r_size_t from, r_size_t to) {
  if (r_kind(y) == r_kind(x)) {
    r_vec_poke_from(x, offset, y, from, to);
    return ;
  }
  if (r_is_object(y)) {
    r_abort("Can't splice S3 objects");
  }

  // FIXME: This callbacks to rlang R coercers with an extra copy.
  SEXP coercer = rlang_vec_coercer(x);
  SEXP call = PROTECT(Rf_lang2(coercer, y));
  SEXP coerced = PROTECT(r_eval(call, R_BaseEnv));

  r_vec_poke_from(x, offset, coerced, from, to);
  FREE(2);
}
