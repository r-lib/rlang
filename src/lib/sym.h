#ifndef RLANG_SYM_H
#define RLANG_SYM_H


#define r_unbound_sym R_UnboundValue
#define r_missing_sym R_MissingArg
#define r_names_sym R_NamesSymbol
#define r_class_sym R_ClassSymbol


sexp* r_new_symbol(sexp* x, int* err);

static inline sexp* r_sym(const char* c_string) {
  return Rf_install(c_string);
}

static inline sexp* r_sym_str(sexp* sym) {
  return PRINTNAME(sym);
}
static inline const char* r_sym_c_str(sexp* sym) {
  return CHAR(PRINTNAME(sym));
}

bool r_is_symbol(sexp* sym, const char* string);
bool r_is_symbol_any(sexp* x, const char** strings, int n);

bool r_is_special_op_sym(sexp* x);

extern sexp* r_dot_environment_sym;
extern sexp* r_function_sym;
extern sexp* r_tilde_sym;


#endif
