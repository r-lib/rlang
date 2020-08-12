#ifndef RLANG_SYM_H
#define RLANG_SYM_H


#define r_syms_unbound R_UnboundValue
#define r_syms_missing R_MissingArg
#define r_syms_names R_NamesSymbol
#define r_syms_class R_ClassSymbol
#define r_syms_dots R_DotsSymbol
#define r_syms_namespace R_DoubleColonSymbol
#define r_syms_namespace3 R_TripleColonSymbol

extern sexp* r_syms_dot_environment;
extern sexp* r_syms_function;
extern sexp* r_syms_srcref;
extern sexp* r_syms_tilde;

extern sexp* r_syms_w;
extern sexp* r_syms_x;
extern sexp* r_syms_y;
extern sexp* r_syms_z;

extern sexp* r_syms_dot_x;
extern sexp* r_syms_dot_y;
extern sexp* r_syms_dot_fn;


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
