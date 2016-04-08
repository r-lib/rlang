#define R_NO_REMAP
#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>

bool is_lazy_load(SEXP x);
bool is_scalar(SEXP x);
bool is_call_to(SEXP x, const char* f);
SEXP rhs(SEXP f);
SEXP findLast(SEXP x);
