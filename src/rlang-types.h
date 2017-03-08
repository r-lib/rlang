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
  null_t  = 0,
  sym_t   = 1,
  node_t  = 2,
  fn_t    = 3,
  env_t   = 4,
  prom_t  = 5,
  lang_t  = 6,
  spc_t   = 7,
  blt_t   = 8,
  str_t   = 9,
  lgl_t   = 10,
  int_t   = 13,
  dbl_t   = 14,
  cpl_t   = 15,
  chr_t   = 16,
  dot_t   = 17,
  any_t   = 18,
  list_t  = 19,
  expr_t  = 20,
  bcode_t = 21,
  ptr_t   = 22,
  wref_t  = 23,
  raw_t   = 24,
  s4_t    = 25
};


} // namespace r
} // namespace rlang

#endif
