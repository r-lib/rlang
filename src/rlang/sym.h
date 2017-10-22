#ifndef RLANG_SYM_H
#define RLANG_SYM_H


static inline
SEXP r_sym(const char* c_string) {
  return Rf_install(c_string);
}


#endif
