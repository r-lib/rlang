#define R_NO_REMAP
#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>

bool is_lazy_load(SEXP x);
bool is_call(SEXP x, const char* f);
bool is_formula(SEXP x);
bool is_fpromise(SEXP x);
SEXP f_rhs_(SEXP f);
SEXP f_lhs_(SEXP f);
SEXP f_env_(SEXP f);
SEXP last_cons(SEXP x);
SEXP make_formula1(SEXP rhs, SEXP env);
SEXP rlang_fun(SEXP sym);
int is_symbolic(SEXP x);
int is_true(SEXP x);
int is_sym(SEXP sym, const char* string);
int is_rlang_prefixed(SEXP x, int (*sym_predicate)(SEXP));
int is_any_call(SEXP x, int (*sym_predicate)(SEXP));
int is_prefixed_call(SEXP x, int (*sym_predicate)(SEXP));
int is_rlang_call(SEXP x, int (*sym_predicate)(SEXP));
