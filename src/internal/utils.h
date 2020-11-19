#ifndef RLANG_INTERNAL_UTILS_H
#define RLANG_INTERNAL_UTILS_H


sexp* new_preserved_empty_list();
void signal_soft_deprecated(const char* msg);
sexp* rlang_ns_get(const char* name);
sexp* rlang_enquo(sexp* sym, sexp* frame);

extern sexp* rlang_ns_env;

__attribute__((noreturn))
static inline
void never_reached(const char* fn) {
  r_abort("Internal error in `%s()`: Reached the unreachable.", fn);
}


#endif
