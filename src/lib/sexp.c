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

bool r_has_name_at(sexp* x, r_ssize i) {
  sexp* nms = r_names(x);
  return
    r_typeof(nms) == r_type_character &&
    r_chr_get(nms, i) != r_empty_str;
}
