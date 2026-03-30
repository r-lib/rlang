// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_RLANG_TYPES_H
#define RLANG_RLANG_TYPES_H

#define R_NO_REMAP

#include <stdbool.h> // IWYU pragma: export
#include <Rinternals.h> // IWYU pragma: export
#include <Rversion.h> // IWYU pragma: export
#include <R_ext/Visibility.h> // IWYU pragma: export

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


// Polyfills for R API

#if R_VERSION < R_Version(4, 5, 0)
static inline
int ANY_ATTRIB(SEXP x) {
  return ATTRIB(x) != R_NilValue;
}
static inline
void CLEAR_ATTRIB(SEXP x) {
  SET_ATTRIB(x, R_NilValue);
  SET_OBJECT(x, 0);
  UNSET_S4_OBJECT(x);
}
#endif

#if R_VERSION < R_Version(4, 6, 0)
static inline
bool rlang_promise_is_forced(r_obj* x) {
  return PRVALUE(x) != R_UnboundValue;
}
// Unwrap nested promises to the innermost one.
// Sets `*forced` to TRUE if the innermost promise is forced.
// Uses Floyd's cycle detection to guard against promise loops.
static inline
r_obj* rlang_promise_unwrap(r_obj* x, bool *forced) {
  r_obj* slow = x;
  bool advance_slow = false;

  while (TRUE) {
    r_obj* expr = PREXPR(x);
    if (TYPEOF(expr) != PROMSXP) {
      *forced = rlang_promise_is_forced(x);
      return x;
    }

    x = expr;
    if (x == slow) {
      Rf_error("Cycle detected in promise chain");
    }

    if (advance_slow) {
      slow = PREXPR(slow);
    }
    advance_slow = !advance_slow;
  }
}
#endif

#if R_VERSION < R_Version(4, 6, 0)
static inline
SEXP R_mapAttrib(SEXP x, SEXP (*FUN)(SEXP, SEXP, void *), void *data) {
  PROTECT_INDEX api;
  SEXP a = ATTRIB(x);
  SEXP val = NULL;

  PROTECT_WITH_INDEX(a, &api);
  while (a != R_NilValue) {
    SEXP tag = PROTECT(TAG(a));
    SEXP attr = PROTECT(CAR(a));
    val = FUN(tag, attr, data);
    UNPROTECT(2);
    if (val != NULL)
      break;
    REPROTECT(a = CDR(a), api);
  }
  UNPROTECT(1);
  return val;
}
#endif


#endif
