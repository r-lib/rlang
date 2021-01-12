#ifndef RLANG_FORMULA_H
#define RLANG_FORMULA_H


bool r_is_formula(sexp* x, int scoped, int lhs);

sexp* r_f_rhs(sexp* f);
sexp* r_f_lhs(sexp* f);
sexp* r_f_env(sexp* f);
bool r_f_has_env(sexp* f);


#endif
