#ifndef RLANG_QUO_H
#define RLANG_QUO_H


sexp* rlang_new_quosure(sexp* expr, sexp* env);
bool rlang_is_quosure(sexp* x);

sexp* rlang_get_expression(sexp* x, sexp* alternate);


static inline bool rlang_quo_is_missing(sexp* quo) {
  return r_node_cadr(quo) == R_MissingArg;
}
static inline bool rlang_quo_is_symbol(sexp* quo) {
  return r_typeof(r_node_cadr(quo)) == r_type_symbol;
}
static inline bool rlang_quo_is_call(sexp* quo) {
  return r_typeof(r_node_cadr(quo)) == r_type_call;
}
static inline bool rlang_quo_is_symbolic(sexp* quo) {
  return r_is_symbolic(r_node_cadr(quo));
}
static inline bool rlang_quo_is_null(sexp* quo) {
  return r_node_cadr(quo) == r_null;
}


#endif
