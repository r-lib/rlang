#include "rlang.h"

SEXP r_set_attribute(SEXP x, SEXP sym, SEXP attr) {
  x = KEEP(r_duplicate(x, true));
  r_poke_attribute(x, sym, attr);

  FREE(1);
  return x;
}

bool r_is_named(SEXP x) {
  SEXP nms = r_names(x);

  if (r_kind(nms) != STRSXP) {
    return false;
  }

  if (r_chr_has(nms, "")) {
    return false;
  }

  return true;
}

bool r_has_name_at(SEXP x, r_size_t i) {
  SEXP nms = r_names(x);
  return
    r_is_character(nms) &&
    !r_chr_has_empty_string_at(nms, i);
}
