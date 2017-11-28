#include "rlang.h"


sexp* r_new_list(sexp* x, const char* name) {
  sexp* out = KEEP(r_new_vector(r_type_list, 1));
  r_list_poke(out, 0, x);

  if (name) {
    sexp* nms = KEEP(r_new_vector(r_type_character, 1));
    r_push_names(x, nms);
    r_chr_poke(nms, 0, r_string(name));
    FREE(1);
  }

  FREE(1);
  return out;
}
