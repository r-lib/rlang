#include "rlang.h"

SEXP set_attr(SEXP x, SEXP sym, SEXP attr) {
  x = KEEP(r_duplicate(x, true));
  mut_attr(x, sym, attr);

  FREE(1);
  return x;
}

bool is_named(SEXP x) {
  SEXP nms = sxp_names(x);

  if (r_kind(nms) != STRSXP) {
    return false;
  }

  if (chr_has(nms, "")) {
    return false;
  }

  return true;
}
