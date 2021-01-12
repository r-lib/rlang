#ifndef RLANG_RLANG_TYPES_H
#define RLANG_RLANG_TYPES_H


#define R_NO_REMAP
#include <Rinternals.h>
#include <R_ext/Visibility.h>

// Use `r_visible` to mark your init function. Then users can compile
// with `-fvisibility=hidden -DHAVE_VISIBILITY_ATTRIBUTE` to link to
// your library (as opposed to dynamically loading it) without risking
// symbol clashes.
#define r_visible attribute_visible extern

typedef struct SEXPREC sexp;
typedef Rcomplex r_complex_t;

typedef R_xlen_t r_ssize;
#define R_SSIZE_MAX R_XLEN_T_MAX
#define R_SSIZE_MIN (-R_XLEN_T_MAX)

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
#define KEEP2(x, y) (KEEP(x), KEEP(y))
#define KEEP_N(x, n) (++(*n), KEEP(x))

#define r_keep_t PROTECT_INDEX
#define KEEP_AT REPROTECT
#define KEEP_HERE PROTECT_WITH_INDEX

#define RLANG_ASSERT(condition) ((void)sizeof(char[1 - 2*!(condition)]))


#endif
