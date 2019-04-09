#ifndef RLANG_LANG_H
#define RLANG_LANG_H

#include "node.h"


#define r_new_call Rf_lcons
#define r_call Rf_lang1
#define r_call2 Rf_lang2
#define r_call3 Rf_lang3

bool r_is_call(sexp* x, const char* name);
bool r_is_call_any(sexp* x, const char** names, int n);

bool r_is_prefixed_call(sexp* x, const char* name);
bool r_is_prefixed_call_any(sexp* x, const char ** names, int n);
bool r_is_maybe_prefixed_call_any(sexp* x, const char ** names, int n);

bool r_is_namespaced_call(sexp* x, const char* ns, const char* name);
bool r_is_namespaced_call_any(sexp* x, const char* ns, const char** names, int n);

bool r_is_special_op_call(sexp* x);

sexp* r_expr_protect(sexp* x);


#endif
