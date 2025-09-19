// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_C_UTILS_H
#define RLANG_C_UTILS_H

#include "rlang-types.h"

#include <inttypes.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include "cnd.h"

#define R_ARR_SIZEOF(X) sizeof(X) / sizeof(X[0])
#define R_MIN(a, b) ((a) < (b) ? (a) : (b))
#define R_MAX(a, b) ((a) > (b) ? (a) : (b))

// Like `memset()` with support for multi-byte types
#define R_MEM_SET(TYPE, PTR, VALUE, N) do {     \
    TYPE* v = (PTR);                            \
    TYPE value = (VALUE);                       \
    size_t n = (N);                             \
    for (size_t i = 0; i < n; ++i) {            \
      v[i] = value;                             \
    }                                           \
  } while(0)


void* r_shelter_deref(r_obj* x);


// Allow integers up to 2^52, same as R_XLEN_T_MAX when long vector
// support is enabled
#define RLANG_MAX_DOUBLE_INT 4503599627370496
#define RLANG_MIN_DOUBLE_INT -4503599627370496

static inline
bool r_dbl_is_whole(double x) {
  if (x > RLANG_MAX_DOUBLE_INT || x < RLANG_MIN_DOUBLE_INT) {
    return false;
  }

  // C99 guarantees existence of the int_least_N_t types, even on
  // machines that don't support arithmetic on width N:
  if (x != (int_least64_t) x) {
    return false;
  }

  return true;
}

// Adapted from CERT C coding standards
static inline
intmax_t r__intmax_add(intmax_t x, intmax_t y) {
  if ((y > 0 && x > (INTMAX_MAX - y)) ||
      (y < 0 && x < (INTMAX_MIN - y))) {
    r_stop_internal("Values too large to be added.");
  }

  return x + y;
}
static inline
intmax_t r__intmax_subtract(intmax_t x, intmax_t y) {
  if ((y > 0 && x < (INTMAX_MIN + y)) ||
      (y < 0 && x > (INTMAX_MAX + y))) {
    r_stop_internal("Subtraction resulted in overflow or underflow.");
  }

  return x - y;
}

static inline
r_ssize r_ssize_add(r_ssize x, r_ssize y) {
  intmax_t out = r__intmax_add(x, y);

  if (out > R_SSIZE_MAX) {
    r_stop_internal("Result too large for an `r_ssize`.");
  }

  return (r_ssize) out;
}

static inline
r_ssize r_ssize_mult(r_ssize x, r_ssize y) {
  if (x > 0) {
    if (y > 0) {
      if (x > (R_SSIZE_MAX / y)) {
        goto error;
      }
    } else {
      if (y < (R_SSIZE_MIN / x)) {
        goto error;
      }
    }
  } else {
    if (y > 0) {
      if (x < (R_SSIZE_MIN / y)) {
        goto error;
      }
    } else {
      if ( (x != 0) && (y < (R_SSIZE_MAX / x))) {
        goto error;
      }
    }
  }

  return x * y;

 error:
  r_stop_internal("Result too large for an `r_ssize`.");
}

static inline
int r_int_min(int x, int y) {
  return (y < x) ? y : x;
}
static inline
int r_int_max(int x, int y) {
  return (y < x) ? x : y;
}

static inline
r_ssize r_ssize_min(r_ssize x, r_ssize y) {
  return (y < x) ? y : x;
}
static inline
r_ssize r_ssize_max(r_ssize x, r_ssize y) {
  return (y < x) ? x : y;
}

static inline
int r_ssize_as_integer(r_ssize x) {
  if (x > INT_MAX || x < INT_MIN) {
    r_stop_internal("Result can't be represented as `int`.");
  }

  return (int) x;
}
static inline
double r_ssize_as_double(r_ssize x) {
  if (x > DBL_MAX || x < -DBL_MAX) {
    r_stop_internal("Result can't be represented as `double`.");
  }

  return (double) x;
}

static inline
r_ssize r_double_as_ssize(double x) {
  if (x > R_SSIZE_MAX || x < R_SSIZE_MIN) {
    r_stop_internal("Result can't be represented as `r_ssize`.");
  }

  return (r_ssize) x;
}

static inline
double r_double_mult(double x, double y) {
  double out = x * y;

  if (!isfinite(out)) {
    r_stop_internal("Can't multiply double values.");
  }

  return out;
}

// Slightly safer version of `memcpy()` for use with R object memory
//
// Prefer this over `memcpy()`, especially when providing pointers to R object
// memory. As of R 4.5.0, `DATAPTR()` and friends return `(void*) 1` on 0-length
// R objects, so we must be extremely careful to never use dereference those
// pointers. In particular, it is not safe to call `memcpy(dest, src, 0)` on
// some machines (likely with sanitizers active) when either `dest` or `src`
// resolve to `(void*) 1`.
//
// https://github.com/r-lib/vctrs/pull/1968
// https://github.com/r-devel/r-svn/blob/9976c3d7f08c754593d01ba8380afb6be803dde2/src/main/memory.c#L4137-L4150
static inline
void r_memcpy(void* dest, const void* src, size_t count) {
  if (count) {
    memcpy(dest, src, count);
  }
}

// Slightly safer version of `memset()` for use with R object memory
//
// See `r_memcpy()` for rationale
static inline
void r_memset(void* dest, int value, size_t count) {
  if (count) {
    memset(dest, value, count);
  }
}

#endif
