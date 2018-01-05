#include <string.h>
#include "rlang.h"


sexp* r_new_character(const char** strings, int n) {
  sexp* out = KEEP(r_new_vector(STRSXP, n));

  for (int i = 0; i < n; ++i) {
    r_chr_poke(out, i, r_string(strings[i]));
  }

  FREE(1);
  return out;
}

bool r_chr_has(sexp* chr, const char* c_string) {
  sexp* nm;
  for (int i = 0; i != r_vec_length(chr); ++i) {
    nm = STRING_ELT(chr, i);
    if (strcmp(CHAR(nm), c_string) == 0) {
      return true;
    }
  }

  return false;
}

static void validate_chr_setter(sexp* chr, sexp* r_string) {
  if (!r_is_character(chr)) {
    r_abort("`chr` must be a character vector");
  }
  if (!r_is_r_string(r_string)) {
    r_abort("`r_string` must be an internal R string");
  }
}
sexp* chr_prepend(sexp* chr, sexp* r_string) {
  if (r_is_null(chr)) {
    return Rf_ScalarString(r_string);
  } else {
    validate_chr_setter(chr, r_string);
  }

  int n = r_length(chr);
  sexp* out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 1, chr, 0, n);
  r_chr_poke(out, 0, r_string);

  FREE(1);
  return out;
}
sexp* chr_append(sexp* chr, sexp* r_str) {
  if (r_is_null(chr)) {
    return Rf_ScalarString(r_str);
  }
  validate_chr_setter(chr, r_str);

  int n = r_length(chr);
  sexp* out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 0, chr, 0, n);
  r_chr_poke(out, n, r_str);

  FREE(1);
  return out;
}
