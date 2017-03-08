#ifndef RLANG_NODE_H
#define RLANG_NODE_H

namespace rlang {


inline
sexp* node_car(sexp* x) {
  return CAR(x);
}

inline
sexp* node_cdr(sexp* x) {
  return CDR(x);
}


} // namespace rlang

#endif
