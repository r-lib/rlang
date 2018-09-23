#ifndef RLANG_VECTOR_LIST_H
#define RLANG_VECTOR_LIST_H


static inline sexp* r_list_get(sexp* list, r_ssize i) {
  return VECTOR_ELT(list, i);
}
static inline void r_list_poke(sexp* list, r_ssize i, sexp* elt) {
  SET_VECTOR_ELT(list, i, elt);
}

sexp* r_new_list(sexp* x, const char* name);

static inline bool r_is_list(sexp* x, r_ssize n) {
  if (r_typeof(x) != r_type_list) {
    return false;
  }
  if (n < 0) {
    return true;
  }
  return r_length(x) == n;
}


#endif
