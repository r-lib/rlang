#include "rlang.h"

void r_list_poke(SEXP list, r_size_t i, SEXP elt) {
  SET_VECTOR_ELT(list, i, elt);
}
