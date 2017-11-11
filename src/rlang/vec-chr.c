#include <string.h>
#include "rlang.h"

bool r_chr_has(SEXP chr, const char* c_string) {
  SEXP nm;
  for (int i = 0; i != r_vec_length(chr); ++i) {
    nm = STRING_ELT(chr, i);
    if (!strcmp(CHAR(nm), c_string)) {
      return true;
    }
  }

  return false;
}

static void validate_chr_setter(SEXP chr, SEXP r_string) {
  if (!is_character(chr))
    r_abort("`chr` must be a character vector");
  if (!r_is_r_string(r_string))
    r_abort("`r_string` must be an internal R string");
}
SEXP chr_prepend(SEXP chr, SEXP r_string) {
  if (r_is_null(chr))
    return Rf_ScalarString(r_string);
  else
    validate_chr_setter(chr, r_string);

  int n = r_length(chr);
  SEXP out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 1, chr, 0, n);
  r_chr_poke(out, 0, r_string);

  FREE(1);
  return out;
}
SEXP chr_append(SEXP chr, SEXP r_str) {
  if (r_is_null(chr)) {
    return Rf_ScalarString(r_str);
  }
  validate_chr_setter(chr, r_str);

  int n = r_length(chr);
  SEXP out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 0, chr, 0, n);
  r_chr_poke(out, n, r_str);

  FREE(1);
  return out;
}
