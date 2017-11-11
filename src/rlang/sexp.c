#include "rlang.h"

SEXP r_set_attribute(SEXP x, SEXP sym, SEXP attr) {
  x = KEEP(r_duplicate(x, true));
  r_poke_attribute(x, sym, attr);

  FREE(1);
  return x;
}

bool r_is_named(SEXP x) {
  SEXP nms = r_get_names(x);

  if (r_kind(nms) != STRSXP) {
    return false;
  }

  if (r_chr_has(nms, "")) {
    return false;
  }

  return true;
}
