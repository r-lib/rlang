#ifndef RLANG_VECTOR_CHR_H
#define RLANG_VECTOR_CHR_H


static inline SEXP r_chr_get(SEXP chr, size_t i) {
  return STRING_ELT(chr, i);
}
static inline void r_chr_poke(SEXP chr, r_size_t i, SEXP elt) {
  SET_STRING_ELT(chr, i, elt);
}

bool r_chr_has(SEXP chr, const char* c_string);


static inline SEXP r_string(const char* c_string) {
  return Rf_mkChar(c_string);
}
static inline bool r_is_r_string(SEXP x) {
  return r_kind(x) == CHARSXP;
}

static inline SEXP r_scalar_chr(const char* c_string) {
  return Rf_mkString(c_string);
}
static inline bool r_is_scalar_character(SEXP x) {
  return r_kind(x) == STRSXP && r_length(x) == 1;
}

static inline const char* r_c_string(SEXP scalar_chr) {
  return CHAR(r_chr_get(scalar_chr, 0));
}


SEXP chr_prepend(SEXP chr, SEXP r_string);
SEXP chr_append(SEXP chr, SEXP r_string);


#endif
