#include <string.h>
#include "rlang.h"

SEXP r_chr_get(SEXP chr, size_t i) {
  return STRING_ELT(chr, i);
}

bool chr_has(SEXP chr, const char* c_string) {
  SEXP nm;
  for (int i = 0; i != r_vec_length(chr); ++i) {
    nm = STRING_ELT(chr, i);
    if (!strcmp(CHAR(nm), c_string))
      return true;
  }

  return false;
}

void mut_chr_at(SEXP chr, r_size_t i, SEXP elt) {
  SET_STRING_ELT(chr, i, elt);
}

SEXP r_string(const char* c_string) {
  return Rf_mkChar(c_string);
}
bool is_r_string(SEXP x) {
  return r_kind(x) == CHARSXP;
}

SEXP string(const char* c_string) {
  return Rf_mkString(c_string);
}
bool is_string(SEXP x) {
  return r_kind(x) == STRSXP && r_length(x) == 1;
}

const char* r_c_string(SEXP scalar_chr) {
  return CHAR(r_chr_get(scalar_chr, 0));
}

static
void validate_chr_setter(SEXP chr, SEXP r_string) {
  if (!is_character(chr))
    r_abort("`chr` must be a character vector");
  if (!is_r_string(r_string))
    r_abort("`r_string` must be an internal R string");
}
SEXP chr_prepend(SEXP chr, SEXP r_string) {
  if (r_is_null(chr))
    return Rf_ScalarString(r_string);
  else
    validate_chr_setter(chr, r_string);

  int n = r_length(chr);
  SEXP out = KEEP(Rf_allocVector(STRSXP, n + 1));

  r_vec_poke_n(out, 1, chr, 0, n);
  mut_chr_at(out, 0, r_string);

  FREE(1);
  return out;
}
SEXP chr_append(SEXP chr, SEXP r_string) {
  if (r_is_null(chr))
    return Rf_ScalarString(r_string);
  else
    validate_chr_setter(chr, r_string);

  int n = r_length(chr);
  SEXP out = KEEP(Rf_allocVector(STRSXP, n + 1));

  r_vec_poke_n(out, 0, chr, 0, n);
  mut_chr_at(out, n, r_string);

  FREE(1);
  return out;
}
