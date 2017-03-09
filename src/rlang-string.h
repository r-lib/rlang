#ifndef RLANG_STRING_H
#define RLANG_STRING_H

#include <cstring>


namespace rlang {

namespace str {

inline
const char* pointer(sexp* str) {
  return CHAR(str);
}

inline
bool is_empty(sexp* str) {
  const char* c_str = pointer(str);
  return strcmp(c_str, "") == 0;
}

} // namespace str


namespace chr {

inline
sexp* as_string(sexp* chr) {
  return get(chr, 0);
}
inline
const char* as_c_string(sexp* chr) {
  return str::pointer(get(chr, 0));
}

} // namespace chr


} // namespace rlang

#endif
