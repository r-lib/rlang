#ifndef RLANG_VECTOR_LIST_H
#define RLANG_VECTOR_LIST_H


static inline SEXP r_list_get(SEXP list, r_size_t i) {
  return VECTOR_ELT(list, i);
}
static inline void r_list_poke(SEXP list, r_size_t i, SEXP elt) {
  SET_VECTOR_ELT(list, i, elt);
}

sexp* r_new_list(sexp* x, const char* name);


#endif
