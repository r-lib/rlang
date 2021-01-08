#ifndef RLANG_INTERNAL_UTILS_H
#define RLANG_INTERNAL_UTILS_H


sexp* new_preserved_empty_list();
sexp* rlang_ns_get(const char* name);
sexp* rlang_enquo(sexp* sym, sexp* frame);

extern sexp* rlang_ns_env;

void signal_soft_deprecated(const char* msg, const char* id, sexp* env);
void warn_deprecated(const char* id, const char* fmt, ...);
void stop_defunct(const char* fmt, ...);

bool r_is_prefixed_call(sexp* x, const char* name);
bool r_is_namespaced_call(sexp* x, const char* ns, const char* name);
bool r_is_namespaced_call_any(sexp* x, const char* ns, const char** names, int n);


#endif
