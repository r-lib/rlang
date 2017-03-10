#ifndef RLANG_NODE_H
#define RLANG_NODE_H

namespace rlang {
namespace node {


inline
sexp* car(sexp* x) {
  return CAR(x);
}

inline
sexp* cdr(sexp* x) {
  return CDR(x);
}


} // namespace node
} // namespace rlang

#endif
