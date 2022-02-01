#ifndef RLANG_LANG_H
#define RLANG_LANG_H

#include "node.h"


#define r_new_call Rf_lcons
#define r_call Rf_lang1
#define r_call2 Rf_lang2
#define r_call3 Rf_lang3
#define r_call4 Rf_lang4
#define r_call5 Rf_lang5

bool r_is_call(r_obj* x, const char* name);
bool r_is_call_any(r_obj* x, const char** names, int n);

r_obj* r_expr_protect(r_obj* x);
r_obj* r_call_clone(r_obj* x);


#endif
