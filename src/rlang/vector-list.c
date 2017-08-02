#include "rlang.h"

void mut_list_at(SEXP list, R_len_t i, SEXP elt) {
  SET_VECTOR_ELT(list, i, elt);
}
