#ifndef RLANG_SYM_H
#define RLANG_SYM_H


static inline SEXP r_sym(const char* c_string) {
  return Rf_install(c_string);
}

int r_is_symbol(SEXP sym, const char* string);
int r_is_symbol_any(SEXP x, const char** strings, int n);


#endif
