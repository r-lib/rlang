#ifndef RLANG_SEXP_H
#define RLANG_SEXP_H

namespace rlang {
namespace sxp {


inline
sexp_e kind(sexp* x) {
  return TYPEOF(x);
}

inline
sexp_e kind(const char* x) {
  return Rf_str2type(x);
}

inline
sexp* attrs(sexp* x) {
  return ATTRIB(x);
}
inline
sexp* attr(sexp* x, sexp* attr_sym) {
  return Rf_getAttrib(x, attr_sym);
}

inline
r::size_t length(sexp* x) {
  return Rf_length(x);
}

inline
bool is_null(sexp* x) {
  return x == r::null;
}
inline
bool is_object(sexp* x) {
  return OBJECT(x) != 0;
}

inline
bool inherits(sexp* x, const char* c) {
  return Rf_inherits(x, c);
}


} // namespace sxp
} // namespace rlang

#endif
