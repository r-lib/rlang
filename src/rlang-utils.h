#ifndef RLANG_UTILS_H
#define RLANG_UTILS_H

namespace rlang {
namespace r {


// Need C++11's variadic templates to pass additional arguments
inline
void abort(const char* msg) {
  Rf_error(msg);
}
inline
void warn(const char* msg) {
  Rf_warning(msg);
}

inline
void printf(const char* msg) {
  Rprintf(msg);
}


} // namespace r
} // namespace rlang

#endif
