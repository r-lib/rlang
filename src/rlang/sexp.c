#include "rlang.h"

bool r_is_named(sexp* x) {
  sexp* nms = r_names(x);

  if (r_typeof(nms) != STRSXP) {
    return false;
  }

  if (r_chr_has(nms, "")) {
    return false;
  }

  return true;
}
