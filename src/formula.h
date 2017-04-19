#ifndef RLANG_FORMULA_H
#define RLANG_FORMULA_H


SEXP rlang_is_formulaish(SEXP x, SEXP scoped, SEXP lhs);
SEXP f_rhs_(SEXP f);
SEXP f_lhs_(SEXP f);
SEXP f_env_(SEXP f);


#endif
