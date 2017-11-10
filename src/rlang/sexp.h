#ifndef RLANG_SEXP_H
#define RLANG_SEXP_H

#include <stdbool.h>


#define r_null R_NilValue
#define r_length Rf_length

static inline
SEXPTYPE r_kind(SEXP x) {
  return TYPEOF(x);
}

SEXP r_get_attr(SEXP x, SEXP sym);

bool r_inherits(SEXP x, const char* class_);

void mut_attr(SEXP x, SEXP sym, SEXP attr);
void mut_class(SEXP x, SEXP classes);

SEXP set_attr(SEXP x, SEXP sym, SEXP attr);
SEXP set_class(SEXP x, SEXP classes);

SEXP sxp_class(SEXP x);
SEXP sxp_names(SEXP x);

void mut_names(SEXP x, SEXP nms);
bool is_named(SEXP x);

SEXP r_missing_arg();
bool r_is_missing(SEXP x);

static inline
bool r_is_null(SEXP x) {
  return x == R_NilValue;
}

SEXP r_duplicate(SEXP x, bool shallow);

static inline
bool r_is_object(SEXP x) {
  return OBJECT(x);
}


#endif
