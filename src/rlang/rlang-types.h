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

#ifdef __GNUC__
# define r_unused __attribute__ ((unused))
#else
# define r_unused
#endif

#define r_no_return __attribute__ ((noreturn))

typedef struct SEXPREC r_obj;
typedef Rcomplex r_complex;

typedef R_xlen_t r_ssize;
#define R_SSIZE_MAX R_XLEN_T_MAX
#define R_SSIZE_MIN (-R_XLEN_T_MAX)

#ifdef LONG_VECTOR_SUPPORT
# define R_PRI_SSIZE "td"
#else
# define R_PRI_SSIZE "d"
#endif

enum r_type {
  R_TYPE_null        = 0,
  R_TYPE_symbol      = 1,
  R_TYPE_pairlist    = 2,
  R_TYPE_closure     = 3,
  R_TYPE_environment = 4,
  R_TYPE_promise     = 5,
  R_TYPE_call        = 6,
  R_TYPE_special     = 7,
  R_TYPE_builtin     = 8,
  R_TYPE_string      = 9,
  R_TYPE_logical     = 10,
  R_TYPE_integer     = 13,
  R_TYPE_double      = 14,
  R_TYPE_complex     = 15,
  R_TYPE_character   = 16,
  R_TYPE_dots        = 17,
  R_TYPE_any         = 18,
  R_TYPE_list        = 19,
  R_TYPE_expression  = 20,
  R_TYPE_bytecode    = 21,
  R_TYPE_pointer     = 22,
  R_TYPE_weakref     = 23,
  R_TYPE_raw         = 24,
  R_TYPE_s4          = 25,

  R_TYPE_new         = 30,
  R_TYPE_free        = 31,

  R_TYPE_function    = 99
};

#define r_null R_NilValue


struct r_pair {
  r_obj* x;
  r_obj* y;
};

struct r_triple {
  r_obj* x;
  r_obj* y;
  r_obj* z;
};

struct r_pair_ptr_ssize {
  void* ptr;
  r_ssize size;
};

struct r_pair_callback {
  r_obj* (*fn)(void* data);
  void* data;
};

struct r_lazy {
  r_obj* x;
  r_obj* env;
};


#define KEEP PROTECT
#define FREE UNPROTECT
#define KEEP2(x, y) (KEEP(x), KEEP(y))
#define KEEP_N(x, n) (++(*n), KEEP(x))

#define r_keep_loc PROTECT_INDEX
#define KEEP_AT REPROTECT
#define KEEP_HERE PROTECT_WITH_INDEX

#define KEEP_WHILE(X, EXPR) do {                \
    KEEP(X);                                    \
    EXPR;                                       \
    FREE(1);                                    \
  } while (0)


#define RLANG_ASSERT(condition) ((void)sizeof(char[1 - 2*!(condition)]))


#endif
