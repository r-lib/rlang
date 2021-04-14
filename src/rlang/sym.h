#ifndef RLANG_SYM_H
#define RLANG_SYM_H


r_obj* r_new_symbol(r_obj* x, int* err);

static inline
r_obj* r_sym(const char* c_string) {
  return Rf_install(c_string);
}

static inline
r_obj* r_sym_string(r_obj* sym) {
  return PRINTNAME(sym);
}
static inline
const char* r_sym_c_string(r_obj* sym) {
  return CHAR(PRINTNAME(sym));
}

static inline
r_obj* r_sym_as_character(r_obj* x) {
  return Rf_ScalarString(PRINTNAME(x));
}


bool r_is_symbol(r_obj* sym, const char* string);
bool r_is_symbol_any(r_obj* x, const char** strings, int n);


#endif
