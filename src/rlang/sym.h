#ifndef RLANG_SYM_H
#define RLANG_SYM_H


sexp* r_new_symbol(sexp* x, int* err);

static inline
sexp* r_sym(const char* c_string) {
  return Rf_install(c_string);
}

static inline
sexp* r_sym_string(sexp* sym) {
  return PRINTNAME(sym);
}
static inline
const char* r_sym_c_string(sexp* sym) {
  return CHAR(PRINTNAME(sym));
}

static inline
sexp* r_sym_as_character(sexp* x) {
  return Rf_ScalarString(PRINTNAME(x));
}


bool r_is_symbol(sexp* sym, const char* string);
bool r_is_symbol_any(sexp* x, const char** strings, int n);


#endif
