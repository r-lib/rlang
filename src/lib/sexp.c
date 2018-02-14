#include "rlang.h"

sexp* r_set_attribute(sexp* x, sexp* sym, sexp* attr) {
  x = KEEP(r_duplicate(x, true));
  r_poke_attribute(x, sym, attr);

  FREE(1);
  return x;
}

bool r_is_named(sexp* x) {
  sexp* nms = r_vec_names(x);

  if (r_typeof(nms) != STRSXP) {
    return false;
  }

  if (r_chr_has(nms, "")) {
    return false;
  }

  return true;
}

bool r_has_name_at(sexp* x, r_ssize_t i) {
  sexp* nms = r_vec_names(x);
  return
    r_is_character(nms) &&
    !r_chr_has_empty_string_at(nms, i);
}
