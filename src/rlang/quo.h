#ifndef RLANG_QUO_H
#define RLANG_QUO_H


SEXP r_new_quosure(SEXP expr, SEXP env);
bool r_quo_is_missing(SEXP x);

static inline bool r_is_quosure(SEXP x) {
  return Rf_inherits(x, "quosure");
}

SEXP r_get_expression(SEXP x, SEXP alternate);


#endif
