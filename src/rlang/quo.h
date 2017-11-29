#ifndef RLANG_QUO_H
#define RLANG_QUO_H


sexp* r_new_quosure(sexp* expr, sexp* env);
bool r_quo_is_missing(sexp* x);

static inline bool r_is_quosure(sexp* x) {
  return Rf_inherits(x, "quosure");
}

sexp* r_get_expression(sexp* x, sexp* alternate);


#endif
