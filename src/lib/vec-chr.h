#ifndef RLANG_VECTOR_CHR_H
#define RLANG_VECTOR_CHR_H

#include <string.h>


#define r_missing_str R_NaString

extern sexp* r_shared_empty_chr;
extern sexp* r_empty_str;


static inline sexp* r_chr_get(sexp* chr, r_ssize i) {
  return STRING_ELT(chr, i);
}
static inline void r_chr_poke(sexp* chr, r_ssize i, sexp* elt) {
  SET_STRING_ELT(chr, i, elt);
}

static inline const char* r_str_deref(sexp* str) {
  return CHAR(str);
}

static inline const char* r_chr_get_c_string(sexp* chr, r_ssize i) {
  return CHAR(r_chr_get(chr, i));
}

static inline sexp* r_nms_get(sexp* nms, r_ssize i) {
  if (nms == r_null) {
    return r_empty_str;
  } else {
    return r_chr_get(nms, i);
  }
}

bool r_chr_has(sexp* chr, const char* c_string);
bool r_chr_has_any(sexp* chr, const char** c_strings);
r_ssize r_chr_detect_index(sexp* chr, const char* c_string);


sexp* r_new_character(const char** strings);

static inline sexp* r_string(const char* c_string) {
  return Rf_mkChar(c_string);
}

static inline sexp* r_chr(const char* c_string) {
  return Rf_mkString(c_string);
}


sexp* chr_prepend(sexp* chr, sexp* r_string);
sexp* chr_append(sexp* chr, sexp* r_string);

sexp* r_nms_are_duplicated(sexp* nms, bool from_last);

sexp* r_str_unserialise_unicode(sexp* r_string);

static inline bool r_is_string(sexp* x, const char* string) {
  if (r_typeof(x) != r_type_character || r_length(x) != 1) {
    return false;
  }
  if (string && strcmp(r_chr_get_c_string(x, 0), string) != 0) {
    return false;
  }
  return true;
}

static inline sexp* r_str_as_symbol(sexp* str) {
  return r_sym(Rf_translateChar(str));
}
static inline sexp* r_str_as_character(sexp* x) {
  return Rf_ScalarString(x);
}

static inline bool r_str_is_name(sexp* str) {
  if (str == r_missing_str) {
    return false;
  }
  if (str == r_empty_str) {
    return false;
  }
  return true;
}


#endif
