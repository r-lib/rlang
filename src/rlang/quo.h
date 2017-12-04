#ifndef RLANG_QUO_H
#define RLANG_QUO_H


sexp* r_new_quosure(sexp* expr, sexp* env);

static inline bool r_quo_is_missing(sexp* quo) {
  return r_node_cadr(quo) == R_MissingArg;
}
static inline bool r_quo_is_symbol(sexp* quo) {
  return r_typeof(r_node_cadr(quo)) == r_type_symbol;
}
static inline bool r_quo_is_call(sexp* quo) {
  return r_typeof(r_node_cadr(quo)) == r_type_call;
}
static inline bool r_quo_is_symbolic(sexp* quo) {
  return r_is_symbolic(r_node_cadr(quo));
}
static inline bool r_quo_is_null(sexp* quo) {
  return r_node_cadr(quo) == r_null;
}

static inline bool r_is_quosure(sexp* x) {
  return Rf_inherits(x, "quosure");
}

sexp* r_get_expression(sexp* x, sexp* alternate);


#endif
