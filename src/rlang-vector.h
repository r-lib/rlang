#ifndef RLANG_VECTOR_H
#define RLANG_VECTOR_H

namespace rlang {


namespace list {

inline
sexp* get(sexp* x, r::size_t index) {
  return VECTOR_ELT(x, index);
}
inline
void set(sexp* x, r::size_t index, sexp* elt) {
  SET_VECTOR_ELT(x, index, elt);
}

} // namespace list


namespace chr {

inline
sexp* get(sexp* x, r::size_t index) {
  return STRING_ELT(x, index);
}
inline
void set(sexp* x, r::size_t index, sexp* elt) {
  SET_STRING_ELT(x, index, elt);
}

} // namespace chr


namespace vec {

template <sexp_e K> struct traits { };
template <> struct traits<r::logical_t> { typedef int type; };
template <> struct traits<r::integer_t> { typedef int type; };
template <> struct traits<r::double_t> { typedef double type ; };
template <> struct traits<r::complex_t> { typedef Rcomplex type; };
// template <> struct traits<r::character_t> { typedef const char** type; };
template <> struct traits<r::bytes_t> { typedef Rbyte type; };
template <> struct traits<r::list_t> { typedef sexp* type ; };


// FIXME: Typed return as soon as we switch to C++11 (need default
// template parameter) so compiler can propagate type information
template <sexp_e Kind> inline
void* pointer(sexp* x) {
  r::abort("Internal error: pointer() not implemented for this type");
}
template <> inline
void* pointer<r::logical_t>(sexp* x) {
  return LOGICAL(x);
}
template <> inline
void* pointer<r::integer_t>(sexp* x) {
  return INTEGER(x);
}
template <> inline
void* pointer<r::double_t>(sexp* x) {
  return REAL(x);
}
template <> inline
void* pointer<r::complex_t>(sexp* x) {
  return COMPLEX(x);
}
template <> inline
void* pointer<r::bytes_t>(sexp* x) {
  return RAW(x);
}
template <> inline
void* pointer<r::character_t>(sexp* x) {
  return STRING_PTR(x);
}
template <> inline
void* pointer<r::list_t>(sexp* x) {
  return VECTOR_PTR(x);
}


inline
sexp* alloc(sexp_e kind, r::size_t n) {
  return Rf_allocVector(kind, n);
}

template <sexp_e Kind> inline
void copy_n(sexp* src, r::size_t n, sexp* dest,
            r::size_t offset_dest = 0,
            r::size_t offset_src = 0) {
  typedef typename traits<Kind>::type T;

  T* src_data = (T*) vec::pointer<Kind>(src);
  T* dest_data = (T*) vec::pointer<Kind>(dest);

  for (r::size_t i = 0; i != n; ++i) {
    dest_data[i + offset_dest] = src_data[i + offset_src];
  }
}

template <> inline
void copy_n<r::character_t>(sexp* src, r::size_t n, sexp* dest,
                            r::size_t offset_dest,
                            r::size_t offset_src) {
  sexp* elt;
  for (r::size_t i = 0; i != n; ++i) {
    elt = chr::get(src, i + offset_src);
    chr::set(dest, i + offset_dest, elt);
  }
}

template <> inline
void copy_n<r::list_t>(sexp* src, r::size_t n, sexp* dest,
                       r::size_t offset_dest,
                       r::size_t offset_src) {
  sexp* elt;
  for (r::size_t i = 0; i != n; ++i) {
    elt = list::get(src, i + offset_src);
    list::set(dest, i + offset_dest, elt);
  }
}

} // namespace vec


namespace sxp {

inline
bool is_atomic(sexp* x) {
  switch (sxp::kind(x)) {
  case r::logical_t:
  case r::integer_t:
  case r::double_t:
  case r::complex_t:
  case r::character_t:
  case r::bytes_t:
    return true;
  default:
    return false;
  }
}

} // namespace sxp


namespace lgl {

inline
bool as_bool(sexp* x) {
  if (sxp::kind(x) != r::logical_t || sxp::length(x) != 1)
    r::abort("Expected a scalar boolean");
  return vec::pointer<r::logical_t>(x);
}

} // namespace lgl


} // namespace rlang

#endif
