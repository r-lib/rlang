#ifndef RLANG_INTERNAL_INTERNAL_H
#define RLANG_INTERNAL_INTERNAL_H


sexp* rlang_constants_env;
void rlang_init_internal();
sexp* rlang_ns_get(const char* name);
sexp* rlang_constants_get(const char* name);


#endif
