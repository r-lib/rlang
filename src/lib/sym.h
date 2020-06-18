#ifndef RLANG_SYM_H
#define RLANG_SYM_H


#define r_unbound_sym R_UnboundValue
#define r_missing_sym R_MissingArg
#define r_names_sym R_NamesSymbol
#define r_class_sym R_ClassSymbol
#define r_dots_sym R_DotsSymbol
#define r_namespace_sym R_DoubleColonSymbol
#define r_namespace3_sym R_TripleColonSymbol

extern sexp* r_dot_environment_sym;
extern sexp* r_function_sym;
extern sexp* r_srcref_sym;
extern sexp* r_tilde_sym;

extern sexp* r_w_sym;
extern sexp* r_x_sym;
extern sexp* r_y_sym;
extern sexp* r_z_sym;

extern sexp* r_dot_x_sym;
extern sexp* r_dot_y_sym;
extern sexp* r_dot_fn_sym;


sexp* r_new_symbol(sexp* x, int* err);

static inline sexp* r_sym(const char* c_string) {
  return Rf_install(c_string);
}

static inline sexp* r_sym_get_string(sexp* sym) {
  return PRINTNAME(sym);
}
static inline const char* r_sym_get_c_string(sexp* sym) {
  return CHAR(PRINTNAME(sym));
}

static inline sexp* r_sym_as_character(sexp* x) {
  return Rf_ScalarString(PRINTNAME(x));
}


bool r_is_symbol(sexp* sym, const char* string);
bool r_is_symbol_any(sexp* x, const char** strings, int n);

bool r_is_special_op_sym(sexp* x);


#endif
