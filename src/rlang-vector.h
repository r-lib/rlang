#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

namespace rlang {


template <sexp_e K> struct vec_traits { };
template <> struct vec_traits<r::lgl_t> { typedef int type; };
template <> struct vec_traits<r::int_t> { typedef int type; };
template <> struct vec_traits<r::dbl_t> { typedef double type ; };
template <> struct vec_traits<r::cpl_t> { typedef Rcomplex type; };
// template <> struct vec_traits<r::chr_t> { typedef const char** type; };
template <> struct vec_traits<r::raw_t> { typedef Rbyte type; };
template <> struct vec_traits<r::list_t> { typedef sexp* type ; };

// FIXME: Typed return as soon as we switch to C++11 (need default
// template parameter) so compiler can propagate type information
template <sexp_e Kind> inline
void* vec_pointer(sexp* x) {
  r::abort("Internal error: vec_pointer() not implemented for this type");
}
template <> inline
void* vec_pointer<r::lgl_t>(sexp* x) {
  return LOGICAL(x);
}
template <> inline
void* vec_pointer<r::int_t>(sexp* x) {
  return INTEGER(x);
}
template <> inline
void* vec_pointer<r::dbl_t>(sexp* x) {
  return REAL(x);
}
template <> inline
void* vec_pointer<r::cpl_t>(sexp* x) {
  return COMPLEX(x);
}
template <> inline
void* vec_pointer<r::raw_t>(sexp* x) {
  return RAW(x);
}
template <> inline
void* vec_pointer<r::chr_t>(sexp* x) {
  return STRING_PTR(x);
}
template <> inline
void* vec_pointer<r::list_t>(sexp* x) {
  return VECTOR_PTR(x);
}

inline
const char* str_pointer(sexp* chr) {
  return CHAR(STRING_ELT(chr, 0));
}

inline
sexp* vec_alloc(sexp_e kind, r::size_t n) {
  return Rf_allocVector(kind, n);
}

inline
bool as_bool(sexp* x) {
  if (sxp::kind(x) != r::lgl_t || sxp::length(x) != 1)
    r::abort("Expected a scalar boolean");
  return vec_pointer<r::lgl_t>(x);
}

inline
bool is_atomic(sexp* x) {
  switch (sxp::kind(x)) {
  case r::lgl_t:
  case r::int_t:
  case r::dbl_t:
  case r::cpl_t:
  case r::chr_t:
  case r::raw_t:
    return true;
  default:
    return false;
  }
}

inline
sexp* list_get(sexp* x, r::size_t index) {
  return VECTOR_ELT(x, index);
}
inline
void list_set(sexp* x, r::size_t index, sexp* elt) {
  SET_VECTOR_ELT(x, index, elt);
}
inline
sexp* chr_get(sexp* x, r::size_t index) {
  return STRING_ELT(x, index);
}
inline
void chr_set(sexp* x, r::size_t index, sexp* elt) {
  SET_STRING_ELT(x, index, elt);
}


template <sexp_e Kind> inline
void vec_copy_n(sexp* src, r::size_t n, sexp* dest,
                r::size_t offset_dest = 0,
                r::size_t offset_src = 0) {
  typedef typename vec_traits<Kind>::type T;

  T* src_data = (T*) vec_pointer<Kind>(src);
  T* dest_data = (T*) vec_pointer<Kind>(dest);

  for (r::size_t i = 0; i != n; ++i) {
    dest_data[i + offset_dest] = src_data[i + offset_src];
  }
}

template <> inline
void vec_copy_n<r::chr_t>(sexp* src, r::size_t n, sexp* dest,
                          r::size_t offset_dest,
                          r::size_t offset_src) {
  sexp* elt;
  for (r::size_t i = 0; i != n; ++i) {
    elt = chr_get(src, i + offset_src);
    chr_set(dest, i + offset_dest, elt);
  }
}

template <> inline
void vec_copy_n<r::list_t>(sexp* src, r::size_t n, sexp* dest,
                           r::size_t offset_dest,
                           r::size_t offset_src) {
  sexp* elt;
  for (r::size_t i = 0; i != n; ++i) {
    elt = list_get(src, i + offset_src);
    list_set(dest, i + offset_dest, elt);
  }
}


} // namespace rlang

#endif
