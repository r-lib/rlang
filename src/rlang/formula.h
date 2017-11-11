#ifndef RLANG_FORMULA_H
#define RLANG_FORMULA_H


bool r_is_formulaish(SEXP x, int scoped, int lhs);

SEXP r_f_rhs(SEXP f);
SEXP r_f_lhs(SEXP f);
SEXP r_f_env(SEXP f);
bool r_f_has_env(SEXP f);


#endif
