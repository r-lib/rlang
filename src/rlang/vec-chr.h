#ifndef RLANG_VECTOR_CHR_H
#define RLANG_VECTOR_CHR_H

#include <string.h>


static inline bool r_is_character(SEXP x) {
  return r_typeof(x) == STRSXP;
}

static inline SEXP r_chr_get(SEXP chr, r_size_t i) {
  return STRING_ELT(chr, i);
}
static inline void r_chr_poke(SEXP chr, r_size_t i, SEXP elt) {
  SET_STRING_ELT(chr, i, elt);
}

static inline SEXP r_nms_get(SEXP nms, r_size_t i) {
  if (nms == r_null) {
    static SEXP empty_str = NULL;
    if (!empty_str) empty_str = Rf_mkChar("");
    return empty_str;
  } else {
    return r_chr_get(nms, i);
  }
}

bool r_chr_has(SEXP chr, const char* c_string);


SEXP r_build_character(const char** strings, int n);

static inline SEXP r_string(const char* c_string) {
  return Rf_mkChar(c_string);
}
static inline bool r_is_r_string(SEXP x) {
  return r_typeof(x) == CHARSXP;
}

static inline SEXP r_scalar_chr(const char* c_string) {
  return Rf_mkString(c_string);
}
static inline SEXP r_as_scalar_chr(SEXP x) {
  return Rf_ScalarString(x);
}

static inline bool r_is_scalar_character(SEXP x) {
  return r_typeof(x) == STRSXP && r_length(x) == 1;
}

static inline const char* r_c_string(SEXP scalar_chr) {
  return CHAR(r_chr_get(scalar_chr, 0));
}


SEXP chr_prepend(SEXP chr, SEXP r_string);
SEXP chr_append(SEXP chr, SEXP r_string);

static inline bool r_is_empty_string(SEXP str) {
  const char* c_str = CHAR(str);
  return strcmp(c_str, "") == 0;
}

static inline bool r_chr_has_empty_string_at(SEXP chr, r_size_t i) {
  return r_is_empty_string(r_chr_get(chr, i));
}

SEXP r_str_unserialise_unicode(SEXP r_string);


#endif
