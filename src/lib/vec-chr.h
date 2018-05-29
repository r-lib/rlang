#ifndef RLANG_VECTOR_CHR_H
#define RLANG_VECTOR_CHR_H

#include <string.h>


#define r_missing_str R_NaString

static inline bool r_is_character(sexp* x) {
  return r_typeof(x) == STRSXP;
}

static inline sexp* r_chr_get(sexp* chr, r_ssize_t i) {
  return STRING_ELT(chr, i);
}
static inline void r_chr_poke(sexp* chr, r_ssize_t i, sexp* elt) {
  SET_STRING_ELT(chr, i, elt);
}

static inline const char* r_chr_get_c_string(sexp* chr, r_ssize_t i) {
  return CHAR(STRING_ELT(chr, i));
}

static inline sexp* r_nms_get(sexp* nms, r_ssize_t i) {
  if (nms == r_null) {
    static sexp* empty_str = NULL;
    if (!empty_str) empty_str = Rf_mkChar("");
    return empty_str;
  } else {
    return r_chr_get(nms, i);
  }
}

bool r_chr_has(sexp* chr, const char* c_string);


sexp* r_new_character(const char** strings, int n);

static inline sexp* r_string(const char* c_string) {
  return Rf_mkChar(c_string);
}
static inline bool r_is_r_string(sexp* x) {
  return r_typeof(x) == CHARSXP;
}

static inline sexp* r_scalar_chr(const char* c_string) {
  return Rf_mkString(c_string);
}
static inline sexp* r_as_scalar_chr(sexp* x) {
  return Rf_ScalarString(x);
}

static inline const char* r_c_string(sexp* scalar_chr) {
  return CHAR(r_chr_get(scalar_chr, 0));
}


sexp* chr_prepend(sexp* chr, sexp* r_string);
sexp* chr_append(sexp* chr, sexp* r_string);

static inline bool r_is_empty_string(sexp* str) {
  const char* c_str = CHAR(str);
  return strcmp(c_str, "") == 0;
}

static inline bool r_chr_has_empty_string_at(sexp* chr, r_ssize_t i) {
  return r_is_empty_string(r_chr_get(chr, i));
}

sexp* r_str_unserialise_unicode(sexp* r_string);

static inline bool r_is_string(sexp* x, const char* string) {
  if (r_typeof(x) != r_type_character || r_length(x) != 1) {
    return false;
  }
  if (string && strcmp(r_c_string(x), string) != 0) {
    return false;
  }
  return true;
}

static inline const char* r_str_as_c_string(sexp* str) {
  return CHAR(str);
}

static inline sexp* r_str_as_symbol(sexp* str) {
  return r_sym(Rf_translateChar(str));
}

static inline bool r_str_is_name(sexp* str) {
  if (str == r_missing_str) {
    return false;
  }
  if (r_is_empty_string(str)) {
    return false;
  }
  return true;
}


#endif
