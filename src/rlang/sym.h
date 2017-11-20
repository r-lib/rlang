#ifndef RLANG_SYM_H
#define RLANG_SYM_H


#define r_unbound_sym R_UnboundValue
#define r_missing_sym R_MissingArg
#define r_names_sym R_NamesSymbol


SEXP r_new_symbol(SEXP x, int* err);

static inline SEXP r_sym(const char* c_string) {
  return Rf_install(c_string);
}

static inline SEXP r_sym_str(SEXP sym) {
  return PRINTNAME(sym);
}

bool r_is_symbol(SEXP sym, const char* string);
bool r_is_symbol_any(SEXP x, const char** strings, int n);

bool r_is_special_op_sym(SEXP x);


#endif
