#ifndef RLANG_RLANG_H
#define RLANG_RLANG_H


#define R_NO_REMAP
#include <stdbool.h>
#include <Rinternals.h>

typedef struct SEXPREC sexp;
typedef R_len_t r_size_t;
typedef Rbyte r_byte_t;
typedef Rcomplex r_complex_t;

enum r_type {
  r_type_null        = 0,
  r_type_symbol      = 1,
  r_type_pairlist    = 2,
  r_type_closure     = 3,
  r_type_environment = 4,
  r_type_promise     = 5,
  r_type_call        = 6,
  r_type_special     = 7,
  r_type_builtin     = 8,
  r_type_string      = 9,
  r_type_logical     = 10,
  r_type_integer     = 13,
  r_type_double      = 14,
  r_type_complex     = 15,
  r_type_character   = 16,
  r_type_dots        = 17,
  r_type_any         = 18,
  r_type_list        = 19,
  r_type_expression  = 20,
  r_type_bytecode    = 21,
  r_type_pointer     = 22,
  r_type_weakref     = 23,
  r_type_raw         = 24,
  r_type_s4          = 25,

  r_type_new         = 30,
  r_type_free        = 31,

  r_type_function    = 99
};


#define r_null R_NilValue

#define KEEP PROTECT
#define FREE UNPROTECT

static inline sexp* KEEP_N(sexp* x, int* n) {
  ++(*n);
  return KEEP(x);
}

#include "sexp.h"

#include "attrs.h"
#include "debug.h"
#include "cnd.h"
#include "env.h"
#include "eval.h"
#include "fn.h"
#include "formula.h"
#include "lang.h"
#include "node.h"
#include "quo.h"
#include "stack.h"
#include "squash.h"
#include "sym.h"
#include "vec.h"
#include "vec-chr.h"
#include "vec-lgl.h"
#include "vec-list.h"


#endif
