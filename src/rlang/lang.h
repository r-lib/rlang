#ifndef RLANG_LANG_H
#define RLANG_LANG_H


SEXP r_new_language_(SEXP head, SEXP tail);
SEXP r_new_language(SEXP head, SEXP tail);

bool r_is_call(SEXP x, const char* name);
bool r_is_call_any(SEXP x, const char** names, int n);

bool r_is_prefixed_call(SEXP x);
bool r_is_prefixed_call_any(SEXP x, const char ** names, int n);
bool r_is_maybe_prefixed_call_any(SEXP x, const char ** names, int n);

bool r_is_namespaced_call(SEXP x, const char* ns);
bool r_is_namespaced_call_any(SEXP x, const char* ns, const char** names, int n);


#endif
