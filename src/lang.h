#ifndef RLANG_LANG_H
#define RLANG_LANG_H


SEXP r_new_language_(SEXP head, SEXP tail);
SEXP r_new_language(SEXP head, SEXP tail);

bool r_is_lang(SEXP x);


#endif
