#define R_NO_REMAP
#include <Rinternals.h>
#include <stdbool.h>

template <SEXPTYPE K> struct vec_traits { };
template <> struct vec_traits<LGLSXP> { typedef int type; };
template <> struct vec_traits<INTSXP> { typedef int type; };
template <> struct vec_traits<REALSXP> { typedef double type ; };
template <> struct vec_traits<CPLXSXP> { typedef Rcomplex type; };
template <> struct vec_traits<RAWSXP> { typedef Rbyte type; };


// FIXME: Typed return as soon as we switch to C++11 (need default
// template parameter) so compiler can propagate type information
template <SEXPTYPE Kind> inline
void* vec_begin(SEXP x) {
  Rf_error("Internal error: vec_begin() not implemented for this type");
}

template <> inline void* vec_begin<LGLSXP>(SEXP x) { return LOGICAL(x); }
template <> inline void* vec_begin<INTSXP>(SEXP x) { return INTEGER(x); }
template <> inline void* vec_begin<REALSXP>(SEXP x) { return REAL(x); }
template <> inline void* vec_begin<CPLXSXP>(SEXP x) { return COMPLEX(x); }
template <> inline void* vec_begin<RAWSXP>(SEXP x) { return RAW(x); }
template <> inline void* vec_begin<STRSXP>(SEXP x) { return STRING_PTR(x); }
template <> inline void* vec_begin<VECSXP>(SEXP x) { return VECTOR_PTR(x); }


// Copy --------------------------------------------------------------

template <SEXPTYPE Kind> inline
void vec_copy_n(SEXP src, R_len_t n, SEXP dest,
                R_len_t offset_dest = 0,
                R_len_t offset_src = 0) {
  typedef typename vec_traits<Kind>::type T;

  T* src_data = (T*) vec_begin<Kind>(src);
  T* dest_data = (T*) vec_begin<Kind>(dest);

  for (R_len_t i = 0; i != n; ++i) {
    dest_data[i + offset_dest] = src_data[i + offset_src];
  }
}

template <> inline
void vec_copy_n<STRSXP>(SEXP src, R_len_t n, SEXP dest,
                        R_len_t offset_dest,
                        R_len_t offset_src) {
  SEXP elt;
  for (R_len_t i = 0; i != n; ++i) {
    elt = STRING_ELT(src, i + offset_src);
    SET_STRING_ELT(dest, i + offset_dest, elt);
  }
}

template <> inline
void vec_copy_n<VECSXP>(SEXP src, R_len_t n, SEXP dest,
                        R_len_t offset_dest,
                        R_len_t offset_src) {
  SEXP elt;
  for (R_len_t i = 0; i != n; ++i) {
    elt = VECTOR_ELT(src, i + offset_src);
    SET_VECTOR_ELT(dest, i + offset_dest, elt);
  }
}


// Coercion ----------------------------------------------------------

SEXP namespace_rlang_sym(SEXP sym) {
  static SEXP rlang_sym = Rf_install("rlang");
  return(Rf_lang3(R_DoubleColonSymbol, rlang_sym, sym));
}

template <SEXPTYPE Kind>
SEXP vec_coercer_sym(void) {
  Rf_error("No coercion implemented for `%s`", Rf_type2str(Kind));
}
template <> SEXP vec_coercer_sym<LGLSXP>() {
  static SEXP sym = Rf_install("as_logical");
  return namespace_rlang_sym(sym);
}
template <> SEXP vec_coercer_sym<INTSXP>() {
  static SEXP sym = Rf_install("as_integer");
  return namespace_rlang_sym(sym);
}
template <> SEXP vec_coercer_sym<REALSXP>() {
  static SEXP sym = Rf_install("as_double");
  return namespace_rlang_sym(sym);
}
template <> SEXP vec_coercer_sym<CPLXSXP>() {
  static SEXP sym = Rf_install("as_complex");
  return namespace_rlang_sym(sym);
}
template <> SEXP vec_coercer_sym<STRSXP>() {
  static SEXP sym = Rf_install("as_character");
  return namespace_rlang_sym(sym);
}
template <> SEXP vec_coercer_sym<RAWSXP>() {
  static SEXP sym = Rf_install("as_bytes");
  return namespace_rlang_sym(sym);
}

template <SEXPTYPE Kind>
void vec_copy_coerce_n(SEXP src, R_len_t n, SEXP dest,
                       R_len_t offset_dest = 0,
                       R_len_t offset_src = 0) {
  if (TYPEOF(src) != Kind) {
    // FIXME: This callbacks to rlang R coercers with an extra copy.
    PROTECT_INDEX ipx;
    SEXP call, coerced;
    PROTECT_WITH_INDEX(call = vec_coercer_sym<Kind>(), &ipx);
    REPROTECT(call = Rf_lang2(call, src), ipx);
    REPROTECT(coerced = Rf_eval(call, R_BaseEnv), ipx);
    vec_copy_n<Kind>(coerced, n, dest, offset_dest, offset_src);
    UNPROTECT(1);
  } else {
    vec_copy_n<Kind>(src, n, dest, offset_dest, offset_src);
  }
}
