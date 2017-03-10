#ifndef RLANG_TYPES_H
#define RLANG_TYPES_H

#include <Rinternals.h>


namespace rlang {

typedef SEXPREC sexp;
typedef SEXPTYPE sexp_e;


namespace r {

typedef R_len_t size_t;
sexp* null = R_NilValue;
sexp* scalar_true = Rf_ScalarLogical(true);
sexp* scalar_false = Rf_ScalarLogical(false);

enum sexp_e {
  null_t        = 0,
  symbol_t      = 1,
  node_t        = 2,
  closure_t     = 3,
  environment_t = 4,
  promise_t     = 5,
  language_t    = 6,
  special_t     = 7,
  builtin_t     = 8,
  string_t      = 9,
  logical_t     = 10,
  integer_t     = 13,
  double_t      = 14,
  complex_t     = 15,
  character_t   = 16,
  dots_t        = 17,
  any_t         = 18,
  list_t        = 19,
  expression_t  = 20,
  bytecode_t    = 21,
  pointer_t     = 22,
  weakref_t     = 23,
  bytes_t       = 24,
  s4_t          = 25
};


} // namespace r
} // namespace rlang

#endif
