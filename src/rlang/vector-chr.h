#ifndef RLANG_VECTOR_CHR_H
#define RLANG_VECTOR_CHR_H


bool chr_has(SEXP chr, const char* c_string);
void mut_chr_at(SEXP chr, R_len_t i, SEXP elt);

bool is_r_string(SEXP x);
SEXP r_string(const char* c_string);

bool is_string(SEXP x);
SEXP string(const char* c_string);

SEXP chr_prepend(SEXP chr, SEXP r_string);
SEXP chr_append(SEXP chr, SEXP r_string);


#endif
